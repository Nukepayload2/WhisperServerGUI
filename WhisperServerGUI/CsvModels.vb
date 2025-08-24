Imports System.Text.Json.Serialization

''' <summary>
''' CSV转写行数据类
''' </summary>
Public Class CsvTranscriptionLine
    ''' <summary>
    ''' 文件名
    ''' </summary>
    Public Property FileName As String

    ''' <summary>
    ''' 行号
    ''' </summary>
    Public Property LineNumber As Integer

    ''' <summary>
    ''' 内容
    ''' </summary>
    Public Property Content As String

    ''' <summary>
    ''' 语言
    ''' </summary>
    Public Property Language As String

    ''' <summary>
    ''' 语言概率
    ''' </summary>
    Public Property LanguageProbability As Double

    Public Sub New()
    End Sub

    Public Sub New(fileName As String, lineNumber As Integer, content As String, language As String, languageProbability As Double)
        Me.FileName = fileName
        Me.LineNumber = lineNumber
        Me.Content = content
        Me.Language = language
        Me.LanguageProbability = languageProbability
    End Sub
End Class

''' <summary>
''' Verbose JSON响应数据类
''' </summary>
Public Class VerboseJsonResponse
    ''' <summary>
    ''' 转写文本
    ''' </summary>
    <JsonPropertyName("text")>
    Public Property Text As String

    ''' <summary>
    ''' 检测到的语言
    ''' </summary>
    <JsonPropertyName("detected_language")>
    Public Property DetectedLanguage As String

    ''' <summary>
    ''' 检测到的语言概率
    ''' </summary>
    <JsonPropertyName("detected_language_probability")>
    Public Property DetectedLanguageProbability As Double
End Class
