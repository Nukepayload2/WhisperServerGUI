Imports System.Collections.Concurrent
Imports System.Collections.Generic
Imports System.IO
Imports System.Net.Http
Imports System.Text
Imports System.Text.Json
Imports System.Text.Json.Serialization
Imports System.Threading
Imports Avalonia.Input
Imports Avalonia.Interactivity
Imports Avalonia.Platform.Storage
Imports Avalonia.Threading
Imports Nukepayload2.Csv

Partial Class MainWindow
    Private Async Function IsServerRunning() As Task(Of Boolean)
        Try
            Dim response = Await httpClient.GetAsync($"http://{settings.Host}:{settings.Port}")
            Return response.IsSuccessStatusCode
        Catch
            Return False
        End Try
    End Function

    Private Function GetBasePath() As String
        Return $"http://{settings.Host}:{settings.Port}{settings.RequestPath}"
    End Function

    ' Inference Tab Event Handlers
    Private Async Sub BrowseFilesButton_Click() Handles DropAreaBorder.PointerPressed
        Dim files = Await OpenFileDialog("Select Audio Files", New FilePickerFileType("Audio Files") With {
                                         .Patterns = {"*.wav", "*.mp3", "*.flac", "*.m4a", "*.ogg"}
                                         })
        DropFiles(files)
    End Sub

    Private Sub DropFiles(files As IEnumerable(Of String))
        If files Is Nothing Then Return
        For Each file In files
            FileListBox.Items.Add(file)
        Next
        UpdateTranscriptionButtonState()
    End Sub

    ' Handle Delete key press to remove selected items
    Private Sub FileListBox_KeyDown(sender As Object, e As KeyEventArgs) Handles FileListBox.KeyDown
        If e.Key = Key.Delete Then
            RemoveSelectedFiles()
        End If
    End Sub

    ' Remove selected files from the list
    Private Sub RemoveSelectedFiles()
        ' Create a list of items to remove (need to do this because we can't modify the collection while iterating)
        Dim itemsToRemove As New List(Of Object)
        For Each item As Object In FileListBox.SelectedItems
            itemsToRemove.Add(item)
        Next

        ' Remove the items
        For Each item As Object In itemsToRemove
            FileListBox.Items.Remove(item)
        Next

        UpdateTranscriptionButtonState()
    End Sub

    ' Clear all files from the list
    Private Sub ClearFilesButton_Click(sender As Object, e As RoutedEventArgs) Handles ClearFilesButton.Click
        FileListBox.Items.Clear()
        UpdateTranscriptionButtonState()
    End Sub

    Private Sub DropAreaBorder_DragEnter(sender As Object, e As DragEventArgs)
        If e.Data.Contains(DataFormats.Files) Then
            e.DragEffects = DragDropEffects.Link
        End If
    End Sub

    Private Sub DropAreaBorder_Drop(sender As Object, e As DragEventArgs)
        If e.Data.Contains(DataFormats.Files) Then
            DropFiles(e.Data.GetFiles.Select(Function(it) it.TryGetLocalPath))
        End If
    End Sub

    Private Sub UpdateTranscriptionButtonState()
        StartTranscriptionButton.IsEnabled = FileListBox.Items.Count > 0 AndAlso serverProcess IsNot Nothing AndAlso Not serverProcess.HasExited
    End Sub

    Private Async Sub StartTranscriptionButton_Click(sender As Object, e As RoutedEventArgs) Handles StartTranscriptionButton.Click
        If FileListBox.Items.Count = 0 Then
            Return
        End If

        cancellationTokenSource = New CancellationTokenSource()
        CancelTranscriptionButton.IsEnabled = True
        StartTranscriptionButton.IsEnabled = False
        TranscriptionProgressBar.Value = 0
        TranscriptionProgressBar.IsIndeterminate = True
        ResultsTextBox.Text = ""
        SaveSettings()
        Dim csvTranscriptionLines As New ConcurrentBag(Of CsvTranscriptionLine)

        Try
            Dim results As New List(Of String)
            Dim totalFiles As Integer = FileListBox.Items.Count
            Dim completedFiles As Integer = 0
            Dim cancel = cancellationTokenSource.Token

            ' Convert FileListBox items to a list for ParallelHelper
            Dim filePaths As New List(Of String)
            For Each item As Object In FileListBox.Items
                filePaths.Add(CType(item, String))
            Next

            ' Create ParallelOptions with concurrency limit
            Dim parallelOptions As New ParallelOptions With {
                .MaxDegreeOfParallelism = settings.Concurrency,
                .CancellationToken = cancel
            }

            ' Use ParallelHelper for better async handling
            Await ForEachAsync(filePaths, parallelOptions,
                Async Function(filePath, cancellationToken) As Task
                    If cancellationToken.IsCancellationRequested Then
                        Return
                    End If

                    Dim result = Await TranscribeFile(filePath, cancellationToken, csvTranscriptionLines)

                    Interlocked.Increment(completedFiles)
                    Await Dispatcher.UIThread.InvokeAsync(
                        Sub()
                            TranscriptionProgressBar.IsIndeterminate = False

                            ' Only add to results and update text box if not outputting to file
                            If Not settings.OutputToFile AndAlso Not settings.OutputToCsv Then
                                results.Add($"File: {Path.GetFileName(filePath)}{Environment.NewLine}{result}{Environment.NewLine}")
                                Dim txt = String.Join(Environment.NewLine, results)
                                ResultsTextBox.Text = txt
                                ResultsTextBox.CaretIndex = txt.Length
                            End If

                            TranscriptionProgressBar.Value = (completedFiles * 100) / totalFiles
                        End Sub)
                End Function)

            If settings.OutputToCsv Then
                If settings.OutputToFile Then
                    Await SaveCsvFileAsync(csvTranscriptionLines)
                Else
                    Dim csvContent = GenerateCsv(csvTranscriptionLines)
                    ResultsTextBox.Text = csvContent
                    ResultsTextBox.CaretIndex = csvContent.Length
                End If
            End If

            TranscriptionProgressBar.Value = 100
        Catch ex As Exception
            ResultsTextBox.Text = $"Error: {ex.Message}"
        Finally
            TranscriptionProgressBar.IsIndeterminate = False
            CancelTranscriptionButton.IsEnabled = False
            StartTranscriptionButton.IsEnabled = True
        End Try
    End Sub

    Private Sub CancelTranscriptionButton_Click(sender As Object, e As RoutedEventArgs) Handles CancelTranscriptionButton.Click
        cancellationTokenSource?.Cancel()
    End Sub

    Private Function GetOutputFileExtension(responseFormat As String) As String
        Select Case responseFormat?.ToLowerInvariant()
            Case "verbose_json"
                Return ".verbose.json"
            Case "json"
                Return ".json"
            Case "text"
                Return ".txt"
            Case "srt"
                Return ".srt"
            Case "vtt"
                Return ".vtt"
            Case Else
                Return ".txt"
        End Select
    End Function

    Private Async Function TranscribeFile(filePath As String, cancellationToken As CancellationToken, csvTranscriptionLines As ConcurrentBag(Of CsvTranscriptionLine)) As Task(Of String)
        Try
            Using content As New MultipartFormDataContent()
                ' Add file
                Dim fileBytes = File.ReadAllBytes(filePath)
                Dim fileContent = New ByteArrayContent(fileBytes)
                content.Add(fileContent, "file", Path.GetFileName(filePath))

                ' Add parameters
                ' If OutputToCsv is enabled, force response format to verbose_json
                Dim responseFormat As String = If(settings.OutputToCsv, "verbose_json", settings.ResponseFormat)
                content.Add(New StringContent(responseFormat), "response_format")

                ' Send request
                Dim basePath = GetBasePath()
                Dim response = Await httpClient.PostAsync(
                    $"{basePath}{If(settings.InferencePath <> Nothing, settings.InferencePath, "/inference")}", content, cancellationToken)
                response.EnsureSuccessStatusCode()

                Dim responseContent = Await response.Content.ReadAsStringAsync(cancellationToken)

                ' Handle CSV output mode
                If settings.OutputToCsv Then
                    ' Parse verbose_json response and add to CSV collection
                    ParseAndAddToCsvCollection(filePath, responseContent, csvTranscriptionLines)
                    Return My.Resources.TranscriptionAddedToCsv
                End If

                ' Check if output to file is enabled
                If Not settings.OutputToFile Then
                    ' Return content as usual
                    Return responseContent
                End If
                ' Generate output file path`
                Dim outputFilePath = Path.Combine(
                    Path.GetDirectoryName(filePath),
                    Path.GetFileNameWithoutExtension(filePath) & GetOutputFileExtension(responseFormat)
                )

                If File.Exists(outputFilePath) AndAlso New FileInfo(outputFilePath).Length > 0 Then
                    Return My.Resources.FileSkippedExists
                End If

                ' Write to file
                Try
                    Await File.WriteAllTextAsync(outputFilePath, responseContent, cancellationToken)
                Catch ex As OperationCanceledException
                    File.Delete(outputFilePath)
                    Throw
                End Try

                ' Return success message instead of content
                Return String.Format(My.Resources.FileSavedTo, outputFilePath)
            End Using
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Return String.Format(My.Resources.ErrorTranscribingFile, ex.Message)
        End Try
    End Function

    ''' <summary>
    ''' 解析verbose_json响应并添加到CSV集合
    ''' </summary>
    ''' <param name="filePath">文件路径</param>
    ''' <param name="jsonContent">JSON内容</param>
    Private Sub ParseAndAddToCsvCollection(filePath As String, jsonContent As String, csvTranscriptionLines As ConcurrentBag(Of CsvTranscriptionLine))
        Try
            ' Deserialize JSON response
            Dim response = JsonSerializer.Deserialize(Of VerboseJsonResponse)(jsonContent)

            ' Split text by line breaks
            Dim lines = response.Text.Split(ControlChars.Lf, StringSplitOptions.TrimEntries Or StringSplitOptions.RemoveEmptyEntries)
            Dim fileName = Path.GetFileName(filePath)

            ' Add each line to CSV collection
            For i = 1 To lines.Length
                Dim csvLine = New CsvTranscriptionLine With {
                    .FileName = fileName,
                    .LineNumber = i,
                    .Content = lines(i - 1),
                    .Language = response.DetectedLanguage,
                    .LanguageProbability = response.DetectedLanguageProbability
                }

                ' Add to shared collection (thread-safe)
                csvTranscriptionLines.Add(csvLine)
            Next
        Catch ex As Exception
            ' Log error but don't throw to avoid breaking the transcription process
            Console.WriteLine(String.Format(My.Resources.ErrorParsingJsonForCsv, ex.Message))
        End Try
    End Sub

    ''' <summary>
    ''' 保存CSV文件
    ''' </summary>
    Private Async Function SaveCsvFileAsync(csvTranscriptionLines As ConcurrentBag(Of CsvTranscriptionLine)) As Task
        If csvTranscriptionLines.IsEmpty Then
            ResultsTextBox.Text = My.Resources.NoCsvDataToSave
            Return
        End If

        Try
            Dim csvContent = GenerateCsv(csvTranscriptionLines)
            ' Open save file dialog
            Dim storageProvider = Me.StorageProvider
            Dim result = Await storageProvider.SaveFilePickerAsync(New FilePickerSaveOptions With {
                .Title = "保存CSV文件",
                .DefaultExtension = ".csv",
                .FileTypeChoices = {New FilePickerFileType("CSV Files") With {.Patterns = {"*.csv"}}}
            })

            If result IsNot Nothing Then
                Dim filePath = result.TryGetLocalPath

                ' Write CSV content with UTF-8 BOM encoding
                Await File.WriteAllTextAsync(filePath, csvContent, Encoding.UTF8)

                ResultsTextBox.Text = String.Format(My.Resources.CsvFileSavedTo, filePath)
            End If
        Catch ex As Exception
            ResultsTextBox.Text = String.Format(My.Resources.ErrorSavingCsvFile, ex.Message)
        End Try
    End Function

    Private Shared Function GenerateCsv(csvTranscriptionLines As ConcurrentBag(Of CsvTranscriptionLine)) As String
        ' Generate CSV content using Nukepayload2.Csv
        Return CsvConvert.SerializeObject(
            csvTranscriptionLines.
            OrderBy(Function(it) it.FileName, StringLogicalComparer.OrdinalIgnoreCase).
            ThenBy(Function(it) it.LineNumber).
            ToArray)
    End Function
End Class
