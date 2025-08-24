Option Strict On

Imports System.Globalization

Public Class StringLogicalComparer
    Implements IComparer(Of String), IEqualityComparer(Of String)

    Private Structure Segment
        Public Index As Integer
        Public Length As Integer
        Public Digits As Boolean
    End Structure

    Public Shared ReadOnly Ordinal As New StringLogicalComparer
    Public Shared ReadOnly OrdinalIgnoreCase As New StringLogicalComparer(StringComparison.OrdinalIgnoreCase, False)
    Public Shared ReadOnly FloatingNumberSensitive As New StringLogicalComparer(StringComparison.Ordinal, True)
    Public Shared ReadOnly FloatingNumberIgnoreCase As New StringLogicalComparer(StringComparison.OrdinalIgnoreCase, True)

    Private ReadOnly _decimalSeparator As String
    Private ReadOnly _decimalSeparatorLen As Integer

    Private ReadOnly _numberFormatInfo As NumberFormatInfo

    Sub New()
        MyClass.New(StringComparison.Ordinal, False)
    End Sub

    Sub New(ignoreCase As StringComparison, floatNumbers As Boolean)
        _Comparison = ignoreCase
        _FloatNumbers = floatNumbers

        _numberFormatInfo = NumberFormatInfo.InvariantInfo
        _decimalSeparator = _numberFormatInfo.NumberDecimalSeparator

        _decimalSeparatorLen = _decimalSeparator.Length
    End Sub

    Public ReadOnly Property Comparison As StringComparison

    Public ReadOnly Property FloatNumbers As Boolean

    Public Function Compare(x As String, y As String) As Integer Implements IComparer(Of String).Compare
        x = If(x, String.Empty)
        y = If(y, String.Empty)

        Dim xIndex = 0
        Dim yIndex = 0

        While xIndex < x.Length OrElse yIndex < y.Length
            Dim xSegment = NextSegment(x, xIndex)
            Dim ySegment = NextSegment(y, yIndex)

            Dim order = CompareSegments(x, xSegment, y, ySegment)
            If order <> 0 Then
                Return order
            End If

            xIndex = xSegment.Index + xSegment.Length
            yIndex = ySegment.Index + ySegment.Length
        End While

        Return 0
    End Function

    Private Function NextSegment(text As String, start As Integer) As Segment
        If text.Length = 0 OrElse text.Length <= start Then
            Return New Segment()
        End If

        Dim index = start
        Dim separatorReached = False
        Dim decimalSeparator = False

        Dim segment As New Segment With {
            .Index = index,
            .Length = 0,
            .Digits = Char.IsDigit(text(index))
        }

        While index < text.Length
            Dim [char] = text(index)

            If Char.IsDigit([char]) Then
                If Not segment.Digits Then
                    Exit While
                End If
            Else
                If segment.Digits Then
                    decimalSeparator = IsDecimalSeparator(text, index)

                    If FloatNumbers AndAlso Not separatorReached AndAlso decimalSeparator Then
                        separatorReached = True
                    Else
                        Exit While
                    End If
                End If
            End If

            If decimalSeparator Then
                segment.Length += _decimalSeparatorLen
                index += _decimalSeparatorLen

                decimalSeparator = False
            Else
                segment.Length += 1
                index += 1
            End If
        End While

        Return segment
    End Function

    Private Function CompareSegments(x As String, xSegment As Segment, y As String, ySegment As Segment) As Integer
        Dim xs = x.AsSpan(xSegment.Index, xSegment.Length)
        Dim ys = y.AsSpan(ySegment.Index, ySegment.Length)

        If xSegment.Digits AndAlso ySegment.Digits Then
            Dim xd As Double, yd As Double
            If Double.TryParse(xs, NumberStyles.Any, _numberFormatInfo, xd) AndAlso
                Double.TryParse(ys, NumberStyles.Any, _numberFormatInfo, yd) Then
                Return xd.CompareTo(yd)
            End If
        End If

        Return xs.CompareTo(ys, _Comparison)
    End Function

    Private Function IsDecimalSeparator(text As String, index As Integer) As Boolean
        If _decimalSeparatorLen = 1 Then
            Return text(index) = _decimalSeparator(0)
        End If

        Dim order = String.Compare(text, index, _decimalSeparator, 0, _decimalSeparatorLen)
        Return order = 0
    End Function

    Public Overloads Function Equals(x As String, y As String) As Boolean Implements IEqualityComparer(Of String).Equals
        Return String.Equals(x, y, _Comparison)
    End Function

    Public Overloads Function GetHashCode(obj As String) As Integer Implements IEqualityComparer(Of String).GetHashCode
        If obj Is Nothing Then
            Return 0
        End If

        Return String.GetHashCode(obj, _Comparison)
    End Function
End Class