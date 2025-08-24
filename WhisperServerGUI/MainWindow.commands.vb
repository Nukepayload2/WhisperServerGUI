Imports Avalonia.Interactivity

Partial Class MainWindow
    ' Generate command line arguments for the server
    Private Function GenerateCommandLineArguments() As String
        ' Build command line arguments
        Dim args = $"-m ""{settings.ModelPath}"" -t {settings.Threads}"

        If settings.Language <> "auto" Then
            args += $" -l {settings.Language}"
        End If

        If settings.Translate Then
            args += " -tr"
        End If

        If settings.Diarize Then
            args += " -di"
        End If

        If settings.MaxContext >= 0 Then
            args += $" -mc {settings.MaxContext}"
        End If

        If settings.WordThreshold <> 0.01 Then
            args += $" -wt {settings.WordThreshold}"
        End If

        If settings.NoFallback Then
            args += " -nf"
        End If

        If settings.NoTimestamps Then
            args += " -nt"
        End If

        ' Advanced settings
        If settings.Processors > 1 Then
            args += $" -p {settings.Processors}"
        End If

        If settings.OffsetTime > 0 Then
            args += $" -ot {settings.OffsetTime}"
        End If

        If settings.OffsetSegment > 0 Then
            args += $" -on {settings.OffsetSegment}"
        End If

        If settings.Duration > 0 Then
            args += $" -d {settings.Duration}"
        End If

        If settings.MaxLength > 0 Then
            args += $" -ml {settings.MaxLength}"
        End If

        If settings.SplitOnWord Then
            args += " -sow"
        End If

        If settings.BestOf > 2 Then
            args += $" -bo {settings.BestOf}"
        End If

        If settings.BeamSize > 0 Then
            args += $" -bs {settings.BeamSize}"
        End If

        If settings.AudioContext > 0 Then
            args += $" -ac {settings.AudioContext}"
        End If

        If settings.EntropyThreshold > 0 Then
            args += $" -et {settings.EntropyThreshold}"
        End If

        If settings.LogProbThreshold > 0 Then
            args += $" -lpt {settings.LogProbThreshold}"
        End If

        If settings.DebugMode Then
            args += " -debug"
        End If

        If settings.TinyDiarize Then
            args += " -tdrz"
        End If

        If settings.PrintSpecial Then
            args += " -ps"
        End If

        If settings.PrintColors Then
            args += " -pc"
        End If

        If settings.PrintRealtime Then
            args += " -pr"
        End If

        If settings.PrintProgress Then
            args += " -pp"
        End If

        If settings.SuppressNonSpeech Then
            args += " -sns"
        End If

        If settings.NoSpeechThreshold > 0 Then
            args += $" -nth {settings.NoSpeechThreshold}"
        End If

        If settings.NoContext Then
            args += " -nc"
        End If

        If settings.NoGPU Then
            args += " -ng"
        End If

        If settings.FlashAttention Then
            args += " -fa"
        End If

        If Not String.IsNullOrEmpty(settings.OpenVINODevice) Then
            args += $" -oved ""{settings.OpenVINODevice}"""
        End If

        If Not String.IsNullOrEmpty(settings.DTW) Then
            args += $" -dtw ""{settings.DTW}"""
        End If

        If settings.VAD Then
            args += " --vad"
        End If

        If Not String.IsNullOrEmpty(settings.VADModel) Then
            args += $" -vm ""{settings.VADModel}"""
        End If

        If settings.VADThreshold > 0 Then
            args += $" -vt {settings.VADThreshold}"
        End If

        If settings.VADMinSpeechDuration > 0 Then
            args += $" -vspd {settings.VADMinSpeechDuration}"
        End If

        If settings.VADMinSilenceDuration > 0 Then
            args += $" -vsd {settings.VADMinSilenceDuration}"
        End If

        If settings.VADMaxSpeechDuration > 0 Then
            args += $" -vmsd {settings.VADMaxSpeechDuration}"
        End If

        If settings.VADSpeechPad > 0 Then
            args += $" -vp {settings.VADSpeechPad}"
        End If

        If settings.VADSamplesOverlap > 0 Then
            args += $" -vo {settings.VADSamplesOverlap}"
        End If

        If settings.ConvertAudio Then
            args += " --convert"
        End If

        If Not String.IsNullOrEmpty(settings.RequestPath) Then
            args += $" --request-path ""{settings.RequestPath}"""
        End If

        If settings.InferencePath <> "/inference" AndAlso settings.InferencePath <> Nothing Then
            args += $" --inference-path ""{settings.InferencePath}"""
        End If

        If Not String.IsNullOrEmpty(settings.PublicPath) Then
            args += $" --public ""{settings.PublicPath}"""
        End If

        If settings.DetectLanguage Then
            args += " -dl"
        End If

        If Not String.IsNullOrEmpty(settings.Prompt) Then
            args += $" --prompt ""{settings.Prompt}"""
        End If

        args += $" --host {settings.Host} --port {settings.Port}"

        Return args
    End Function

    Private Sub UpdatePreviewButton_Click() Handles UpdatePreviewButton.Click, ExpanderParameterPreview.Expanded
        ' Save current settings
        SaveSettings()

        ' Generate command line arguments
        Dim args = GenerateCommandLineArguments()

        ' Display in preview text box
        PreviewTextBox.Text = $"{settings.ServerPath}" & Environment.NewLine & args
    End Sub
End Class
