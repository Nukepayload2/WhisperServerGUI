Imports System.IO
Imports System.Text.Json
Imports Avalonia.Interactivity
Imports Avalonia.Platform.Storage

Partial Class MainWindow
    ' Configuration settings
    Private Class AppSettings
        Public Property ServerPath As String
        Public Property ModelPath As String
        Public Property Threads As Integer
        Public Property Language As String
        Public Property Translate As Boolean
        Public Property Diarize As Boolean
        Public Property MaxContext As Integer
        Public Property WordThreshold As Double
        Public Property NoFallback As Boolean
        Public Property NoTimestamps As Boolean
        Public Property Host As String
        Public Property Port As Integer

        ' Advanced settings
        Public Property Processors As Integer
        Public Property OffsetTime As Integer
        Public Property OffsetSegment As Integer
        Public Property Duration As Integer
        Public Property MaxLength As Integer
        Public Property SplitOnWord As Boolean
        Public Property BestOf As Integer
        Public Property BeamSize As Integer
        Public Property AudioContext As Integer
        Public Property EntropyThreshold As Double
        Public Property LogProbThreshold As Double
        Public Property DebugMode As Boolean
        Public Property TinyDiarize As Boolean
        Public Property PrintSpecial As Boolean
        Public Property PrintColors As Boolean
        Public Property PrintRealtime As Boolean
        Public Property PrintProgress As Boolean
        Public Property SuppressNonSpeech As Boolean
        Public Property NoSpeechThreshold As Double
        Public Property NoContext As Boolean
        Public Property NoGPU As Boolean
        Public Property FlashAttention As Boolean
        Public Property OpenVINODevice As String
        Public Property DTW As String
        Public Property VAD As Boolean
        Public Property VADModel As String
        Public Property VADThreshold As Double
        Public Property VADMinSpeechDuration As Integer
        Public Property VADMinSilenceDuration As Integer
        Public Property VADMaxSpeechDuration As Double
        Public Property VADSpeechPad As Integer
        Public Property VADSamplesOverlap As Double
        Public Property ConvertAudio As Boolean
        Public Property RequestPath As String
        Public Property InferencePath As String
        Public Property PublicPath As String
        Public Property DetectLanguage As Boolean
        Public Property Prompt As String
        Public Property ResponseFormat As String
        Public Property OutputToFile As Boolean
        Public Property OutputToCsv As Boolean
        Public Property Concurrency As Integer
    End Class

    Private settings As AppSettings

    Private Sub LoadSettings()
        settings = New AppSettings With {
            .ServerPath = "",
            .ModelPath = "",
            .Threads = 4,
            .Language = "Auto",
            .Translate = False,
            .Diarize = False,
            .MaxContext = -1,
            .WordThreshold = 0.01,
            .NoFallback = False,
            .NoTimestamps = False,
            .Host = "127.0.0.1",
            .Port = 8080,
            .Processors = 1,
            .OffsetTime = 0,
            .OffsetSegment = 0,
            .Duration = 0,
            .MaxLength = 0,
            .SplitOnWord = False,
            .BestOf = 2,
            .BeamSize = -1,
            .AudioContext = 0,
            .EntropyThreshold = 2.4,
            .LogProbThreshold = -1.0,
            .DebugMode = False,
            .TinyDiarize = False,
            .PrintSpecial = False,
            .PrintColors = False,
            .PrintRealtime = False,
            .PrintProgress = False,
            .SuppressNonSpeech = False,
            .NoSpeechThreshold = 0.6,
            .NoContext = False,
            .NoGPU = False,
            .FlashAttention = False,
            .OpenVINODevice = "",
            .DTW = "",
            .VAD = False,
            .VADModel = "",
            .VADThreshold = 0.5,
            .VADMinSpeechDuration = 250,
            .VADMinSilenceDuration = 100,
            .VADMaxSpeechDuration = 0,
            .VADSpeechPad = 30,
            .VADSamplesOverlap = 0.1,
            .ConvertAudio = False,
            .RequestPath = "",
            .InferencePath = "/inference",
            .PublicPath = "",
            .DetectLanguage = False,
            .Prompt = "",
            .ResponseFormat = "json",
            .Concurrency = 1,
            .OutputToFile = True,
            .OutputToCsv = False
        }

        ' Try to load settings from file
        Dim settingsPath = Path.Combine(Path.GetDirectoryName(Environment.ProcessPath), "settings.json")
        If File.Exists(settingsPath) Then
            Try
                Dim json = File.ReadAllText(settingsPath)
                settings = JsonSerializer.Deserialize(Of AppSettings)(json)
            Catch ex As Exception
                ' Use default settings if file can't be read
            End Try
        End If

        ' Update UI with loaded settings
        ServerPathTextBox.Text = settings.ServerPath
        ModelPathTextBox.Text = settings.ModelPath
        ThreadsNumericUpDown.Value = settings.Threads
        LanguageComboBox.SelectedIndex = GetLanguageIndex(settings.Language)
        TranslateCheckBox.IsChecked = settings.Translate
        DiarizeCheckBox.IsChecked = settings.Diarize
        MaxContextNumericUpDown.Value = settings.MaxContext
        WordThresholdNumericUpDown.Value = settings.WordThreshold
        NoFallbackCheckBox.IsChecked = settings.NoFallback
        NoTimestampsCheckBox.IsChecked = settings.NoTimestamps
        HostTextBox.Text = settings.Host
        PortNumericUpDown.Value = settings.Port

        ' Update advanced settings UI
        ProcessorsNumericUpDown.Value = settings.Processors
        OffsetTimeNumericUpDown.Value = settings.OffsetTime
        OffsetSegmentNumericUpDown.Value = settings.OffsetSegment
        DurationNumericUpDown.Value = settings.Duration
        MaxLengthNumericUpDown.Value = settings.MaxLength
        SplitOnWordCheckBox.IsChecked = settings.SplitOnWord
        BestOfNumericUpDown.Value = settings.BestOf
        BeamSizeNumericUpDown.Value = settings.BeamSize
        AudioContextNumericUpDown.Value = settings.AudioContext
        EntropyThresholdNumericUpDown.Value = settings.EntropyThreshold
        LogProbThresholdNumericUpDown.Value = settings.LogProbThreshold
        DebugModeCheckBox.IsChecked = settings.DebugMode
        TinyDiarizeCheckBox.IsChecked = settings.TinyDiarize
        PrintSpecialCheckBox.IsChecked = settings.PrintSpecial
        PrintColorsCheckBox.IsChecked = settings.PrintColors
        PrintRealtimeCheckBox.IsChecked = settings.PrintRealtime
        PrintProgressCheckBox.IsChecked = settings.PrintProgress
        SuppressNonSpeechCheckBox.IsChecked = settings.SuppressNonSpeech
        NoSpeechThresholdNumericUpDown.Value = settings.NoSpeechThreshold
        NoContextCheckBox.IsChecked = settings.NoContext
        NoGPUCheckBox.IsChecked = settings.NoGPU
        FlashAttentionCheckBox.IsChecked = settings.FlashAttention
        OpenVINODeviceTextBox.Text = settings.OpenVINODevice
        DTWTextBox.Text = settings.DTW
        VADCheckBox.IsChecked = settings.VAD
        VADModelTextBox.Text = settings.VADModel
        VADThresholdNumericUpDown.Value = settings.VADThreshold
        VADMinSpeechDurationNumericUpDown.Value = settings.VADMinSpeechDuration
        VADMinSilenceDurationNumericUpDown.Value = settings.VADMinSilenceDuration
        VADMaxSpeechDurationNumericUpDown.Value = settings.VADMaxSpeechDuration
        VADSpeechPadNumericUpDown.Value = settings.VADSpeechPad
        VADSamplesOverlapNumericUpDown.Value = settings.VADSamplesOverlap
        ConvertAudioCheckBox.IsChecked = settings.ConvertAudio
        RequestPathTextBox.Text = settings.RequestPath
        InferencePathTextBox.Text = settings.InferencePath
        PublicPathTextBox.Text = settings.PublicPath
        DetectLanguageCheckBox.IsChecked = settings.DetectLanguage
        PromptTextBox.Text = settings.Prompt

        ' Update response format UI
        Select Case settings.ResponseFormat?.ToLowerInvariant()
            Case "verbose_json"
                ResponseFormatComboBox.SelectedIndex = 0
            Case "json"
                ResponseFormatComboBox.SelectedIndex = 1
            Case "text"
                ResponseFormatComboBox.SelectedIndex = 2
            Case "srt"
                ResponseFormatComboBox.SelectedIndex = 3
            Case "vtt"
                ResponseFormatComboBox.SelectedIndex = 4
            Case Else
                ResponseFormatComboBox.SelectedIndex = 1 ' Default to JSON
        End Select

        ' Update output to file UI
        OutputToFileCheckBox.IsChecked = settings.OutputToFile
        OutputToCsvCheckBox.IsChecked = settings.OutputToCsv
        
        ' Update concurrency UI
        ConcurrencyNumericUpDown.Value = settings.Concurrency
    End Sub

    Private Function GetLanguageIndex(language As String) As Integer
        Select Case language.ToLowerInvariant()
            Case "auto" : Return 0
            Case "en" : Return 1
            Case "zh" : Return 2
            Case "es" : Return 3
            Case "fr" : Return 4
            Case "de" : Return 5
            Case "ja" : Return 6
            Case "ko" : Return 7
            Case Else : Return 0
        End Select
    End Function

    Private Function GetSelectedLanguage() As String
        Select Case LanguageComboBox.SelectedIndex
            Case 0 : Return "auto"
            Case 1 : Return "en"
            Case 2 : Return "zh"
            Case 3 : Return "es"
            Case 4 : Return "fr"
            Case 5 : Return "de"
            Case 6 : Return "ja"
            Case 7 : Return "ko"
            Case Else : Return "auto"
        End Select
    End Function

    Private Sub SaveSettings()
        settings.ServerPath = ServerPathTextBox.Text
        settings.ModelPath = ModelPathTextBox.Text
        settings.Threads = CInt(ThreadsNumericUpDown.Value)
        settings.Language = If(LanguageComboBox.SelectedIndex >= 0, GetSelectedLanguage(), "auto")
        settings.Translate = TranslateCheckBox.IsChecked.GetValueOrDefault()
        settings.Diarize = DiarizeCheckBox.IsChecked.GetValueOrDefault()
        settings.MaxContext = CInt(MaxContextNumericUpDown.Value)
        settings.WordThreshold = WordThresholdNumericUpDown.Value
        settings.NoFallback = NoFallbackCheckBox.IsChecked.GetValueOrDefault()
        settings.NoTimestamps = NoTimestampsCheckBox.IsChecked.GetValueOrDefault()
        settings.Host = HostTextBox.Text
        settings.Port = CInt(PortNumericUpDown.Value)

        ' Save advanced settings
        settings.Processors = CInt(ProcessorsNumericUpDown.Value)
        settings.OffsetTime = CInt(OffsetTimeNumericUpDown.Value)
        settings.OffsetSegment = CInt(OffsetSegmentNumericUpDown.Value)
        settings.Duration = CInt(DurationNumericUpDown.Value)
        settings.MaxLength = CInt(MaxLengthNumericUpDown.Value)
        settings.SplitOnWord = SplitOnWordCheckBox.IsChecked.GetValueOrDefault()
        settings.BestOf = CInt(BestOfNumericUpDown.Value)
        settings.BeamSize = CInt(BeamSizeNumericUpDown.Value)
        settings.AudioContext = CInt(AudioContextNumericUpDown.Value)
        settings.EntropyThreshold = EntropyThresholdNumericUpDown.Value
        settings.LogProbThreshold = LogProbThresholdNumericUpDown.Value
        settings.DebugMode = DebugModeCheckBox.IsChecked.GetValueOrDefault()
        settings.TinyDiarize = TinyDiarizeCheckBox.IsChecked.GetValueOrDefault()
        settings.PrintSpecial = PrintSpecialCheckBox.IsChecked.GetValueOrDefault()
        settings.PrintColors = PrintColorsCheckBox.IsChecked.GetValueOrDefault()
        settings.PrintRealtime = PrintRealtimeCheckBox.IsChecked.GetValueOrDefault()
        settings.PrintProgress = PrintProgressCheckBox.IsChecked.GetValueOrDefault()
        settings.SuppressNonSpeech = SuppressNonSpeechCheckBox.IsChecked.GetValueOrDefault()
        settings.NoSpeechThreshold = NoSpeechThresholdNumericUpDown.Value
        settings.NoContext = NoContextCheckBox.IsChecked.GetValueOrDefault()
        settings.NoGPU = NoGPUCheckBox.IsChecked.GetValueOrDefault()
        settings.FlashAttention = FlashAttentionCheckBox.IsChecked.GetValueOrDefault()
        settings.OpenVINODevice = OpenVINODeviceTextBox.Text
        settings.DTW = DTWTextBox.Text
        settings.VAD = VADCheckBox.IsChecked.GetValueOrDefault()
        settings.VADModel = VADModelTextBox.Text
        settings.VADThreshold = VADThresholdNumericUpDown.Value
        settings.VADMinSpeechDuration = CInt(VADMinSpeechDurationNumericUpDown.Value)
        settings.VADMinSilenceDuration = CInt(VADMinSilenceDurationNumericUpDown.Value)
        settings.VADMaxSpeechDuration = VADMaxSpeechDurationNumericUpDown.Value
        settings.VADSpeechPad = CInt(VADSpeechPadNumericUpDown.Value)
        settings.VADSamplesOverlap = VADSamplesOverlapNumericUpDown.Value
        settings.ConvertAudio = ConvertAudioCheckBox.IsChecked.GetValueOrDefault()
        settings.RequestPath = RequestPathTextBox.Text
        settings.InferencePath = InferencePathTextBox.Text
        settings.PublicPath = PublicPathTextBox.Text
        settings.DetectLanguage = DetectLanguageCheckBox.IsChecked.GetValueOrDefault()
        settings.Prompt = PromptTextBox.Text

        ' Save response format setting
        Select Case ResponseFormatComboBox.SelectedIndex
            Case 0
                settings.ResponseFormat = "verbose_json"
            Case 1
                settings.ResponseFormat = "json"
            Case 2
                settings.ResponseFormat = "text"
            Case 3
                settings.ResponseFormat = "srt"
            Case 4
                settings.ResponseFormat = "vtt"
            Case Else
                settings.ResponseFormat = "json"
        End Select

        ' Save output to file setting
        settings.OutputToFile = OutputToFileCheckBox.IsChecked.GetValueOrDefault()
        settings.OutputToCsv = OutputToCsvCheckBox.IsChecked.GetValueOrDefault()
        
        ' Save concurrency setting
        settings.Concurrency = CInt(ConcurrencyNumericUpDown.Value)

        ' Save settings to file
        Dim settingsDir = Path.GetDirectoryName(Environment.ProcessPath)
        Dim settingsPath = Path.Combine(settingsDir, "settings.json")
        Try
            Dim json = JsonSerializer.Serialize(settings, New JsonSerializerOptions With {.WriteIndented = True})
            File.WriteAllText(settingsPath, json)
        Catch ex As Exception
            ' Ignore save errors
        End Try
    End Sub

    ' Configuration Tab Event Handlers
    Private Async Sub BrowseServerButton_Click(sender As Object, e As RoutedEventArgs) Handles BrowseServerButton.Click
        Dim files = Await OpenFileDialog("Select whisper-server.exe", New FilePickerFileType("Executable Files") With {.Patterns = New String() {"*.exe"}})
        If files IsNot Nothing AndAlso files.Length > 0 Then
            ServerPathTextBox.Text = files(0)
        End If
    End Sub

    Private Async Sub BrowseModelButton_Click(sender As Object, e As RoutedEventArgs) Handles BrowseModelButton.Click
        Dim files = Await OpenFileDialog("Select Model File", New FilePickerFileType("Model Files") With {.Patterns = New String() {"*.bin", "*.gguf"}})
        If files IsNot Nothing AndAlso files.Length > 0 Then
            ModelPathTextBox.Text = files(0)
        End If
    End Sub

    Private Async Sub BrowseDTWButton_Click(sender As Object, e As RoutedEventArgs) Handles BrowseDTWButton.Click
        Dim files = Await OpenFileDialog("Select DTW Model File", New FilePickerFileType("Model Files") With {.Patterns = New String() {"*.bin", "*.gguf"}})
        If files IsNot Nothing AndAlso files.Length > 0 Then
            DTWTextBox.Text = files(0)
        End If
    End Sub

    Private Async Sub BrowseVADModelButton_Click(sender As Object, e As RoutedEventArgs) Handles BrowseVADModelButton.Click
        Dim files = Await OpenFileDialog("Select VAD Model File", New FilePickerFileType("Model Files") With {.Patterns = New String() {"*.bin", "*.gguf"}})
        If files IsNot Nothing AndAlso files.Length > 0 Then
            VADModelTextBox.Text = files(0)
        End If
    End Sub

    Private Async Sub BrowsePublicPathButton_Click(sender As Object, e As RoutedEventArgs) Handles BrowsePublicPathButton.Click
        Dim folders = Await OpenFolderDialog("Select Public Folder")
        If Not String.IsNullOrEmpty(folders) Then
            PublicPathTextBox.Text = folders
        End If
    End Sub

End Class
