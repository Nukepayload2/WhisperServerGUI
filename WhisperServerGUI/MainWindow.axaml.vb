Imports Avalonia.Controls
Imports Avalonia.Interactivity
Imports Avalonia.Platform.Storage
Imports System.Collections.Concurrent
Imports System.IO
Imports System.Net.Http
Imports System.Threading

Partial Class MainWindow
    Inherits Window

    Private serverProcess As Process
    Private httpClient As HttpClient
    Private cancellationTokenSource As CancellationTokenSource
    Private serverRunning As Boolean = False

    Public Sub New()
        InitializeComponent()
        InitializeHttpClient()
        LoadSettings()
    End Sub

    Private Sub InitializeHttpClient()
        httpClient = New HttpClient()
        httpClient.Timeout = TimeSpan.FromMinutes(60) ' Long timeout for transcription
    End Sub

    Private Sub BrowseServerUrlButton_Click(sender As Object, e As RoutedEventArgs) Handles BrowseServerUrlButton.Click
        ' Open the server URL in the default browser
        Try
            Dim url = GetBasePath()
            Process.Start(New ProcessStartInfo(url) With {.UseShellExecute = True})
        Catch ex As Exception
            ' Handle any errors that occur when trying to open the URL
            ServerStatusTextBlock.Text = $"Error opening browser: {ex.Message}"
        End Try
    End Sub

    Private Async Function OpenFolderDialog(title As String) As Task(Of String)
        Dim storageProvider = Me.StorageProvider
        Dim result = Await storageProvider.OpenFolderPickerAsync(New FolderPickerOpenOptions With {
            .Title = title
        })

        If result.Count > 0 Then
            Return result(0).Path.LocalPath
        End If

        Return Nothing
    End Function

    Private Async Sub StartServerButton_Click(sender As Object, e As RoutedEventArgs) Handles StartServerButton.Click
        SaveSettings()

        ' Clear previous output
        ServerOutputListBox.Items.Clear()

        If String.IsNullOrEmpty(settings.ServerPath) OrElse Not File.Exists(settings.ServerPath) Then
            ServerStatusTextBlock.Text = "Server Status: Error - Server executable not found"
            ServerOutputListBox.Items.Add("Error: Server executable not found" & Environment.NewLine & settings.ServerPath)
            Return
        End If

        If String.IsNullOrEmpty(settings.ModelPath) OrElse Not File.Exists(settings.ModelPath) Then
            ServerStatusTextBlock.Text = "Server Status: Error - Model file not found"
            ServerOutputListBox.Items.Add("Error: Model file not found" & Environment.NewLine & settings.ModelPath)
            Return
        End If

        Try
            ' Build command line arguments using the new method
            Dim args = GenerateCommandLineArguments()

            ' Start the server process
            Dim startInfo As New ProcessStartInfo(settings.ServerPath, args) With {
                .UseShellExecute = False,
                .RedirectStandardOutput = True,
                .RedirectStandardError = True,
                .CreateNoWindow = True
            }

            serverProcess = Process.Start(startInfo)

            ' Start reading output streams
            Dim outputTask = ReadOutputStream(serverProcess.StandardOutput, "stdout")
            Dim errorTask = ReadOutputStream(serverProcess.StandardError, "stderr")

            ' Update UI
            StartServerButton.IsEnabled = False
            StopServerButton.IsEnabled = True
            BrowseServerUrlButton.IsEnabled = False ' Disable browse button until server is confirmed running
            ServerStatusTextBlock.Text = "Server Status: Starting..."

            ' Wait a moment for the server to start and check status periodically
            serverRunning = False
            For i As Integer = 0 To 99 ' 100 times
                Await Task.Delay(200) ' 200ms delay
                If Await IsServerRunning() Then
                    serverRunning = True
                    Exit For
                End If
            Next

            If serverRunning Then
                ServerStatusTextBlock.Text = "Server Status: Running"
                StartTranscriptionButton.IsEnabled = True
                BrowseServerUrlButton.IsEnabled = True ' Enable browse button when server is running
            Else
                ServerStatusTextBlock.Text = "Server Status: Error - Server not responding after 20 seconds"
                BrowseServerUrlButton.IsEnabled = False ' Keep browse button disabled if server failed to start
            End If
        Catch ex As Exception
            ServerStatusTextBlock.Text = $"Server Status: Error - {ex.Message}"
            ServerOutputListBox.Items.Add($"Error: {ex.Message}")
            BrowseServerUrlButton.IsEnabled = False ' Keep browse button disabled on error
        End Try
    End Sub

    Private Sub StopServerButton_Click() Handles StopServerButton.Click
        Try
            If serverProcess IsNot Nothing AndAlso Not serverProcess.HasExited Then
                serverProcess.Kill()
                serverProcess.WaitForExit()
            End If
        Catch ex As Exception
            ' Ignore errors when stopping
        Finally
            serverProcess = Nothing
            StartServerButton.IsEnabled = True
            StopServerButton.IsEnabled = False
            BrowseServerUrlButton.IsEnabled = False ' Disable browse button when server is stopped
            ServerStatusTextBlock.Text = "Server Status: Stopped"
            StartTranscriptionButton.IsEnabled = False
        End Try
    End Sub

    Private Async Function OpenFileDialog(title As String, fileType As FilePickerFileType) As Task(Of String())
        Dim storageProvider = Me.StorageProvider
        Dim result = Await storageProvider.OpenFilePickerAsync(New FilePickerOpenOptions With {
            .Title = title,
            .AllowMultiple = True,
            .FileTypeFilter = {fileType}
        })

        If result.Count > 0 Then
            Return result.Select(Function(f) f.Path.LocalPath).ToArray()
        End If

        Return Nothing
    End Function

    Protected Overrides Sub OnClosed(e As EventArgs)
        ' Clean up resources
        StopServerButton_Click()
        httpClient?.Dispose()
        cancellationTokenSource?.Dispose()
        MyBase.OnClosed(e)
    End Sub

    Private Sub MainWindow_Closing(sender As Object, e As WindowClosingEventArgs) Handles Me.Closing
        SaveSettings()
    End Sub

    ' Read output stream asynchronously and display in the output list box
    Private Async Function ReadOutputStream(stream As StreamReader, prefix As String) As Task
        Try
            Dim line As String
            Do
                line = Await stream.ReadLineAsync()
                If line IsNot Nothing Then
                    ServerOutputListBox.Items.Add($"[{prefix}] {line}")
                    ' Auto-scroll to bottom
                    If ServerOutputListBox.Items.Count > 0 Then
                        ServerOutputListBox.ScrollIntoView(ServerOutputListBox.Items.Count - 1)
                    End If
                End If
            Loop While line IsNot Nothing
        Catch ex As Exception
            ' Handle any errors that occur while reading the stream
            ServerOutputListBox.Items.Add($"[{prefix}] Error reading stream: {ex.Message}")
            ' Auto-scroll to bottom
            If ServerOutputListBox.Items.Count > 0 Then
                ServerOutputListBox.ScrollIntoView(ServerOutputListBox.Items.Count - 1)
            End If
        End Try
    End Function
End Class
