Public Class RandomLine
    Shared rrr As New Random()

    Private Base, CurVal, Dur, MyEnd, MyInc, MyRange As Double

    Public Sub New(sBase As Double, sRange As Double)
        CurVal = 0.0
        Reset()
        Base = sBase
        MyRange = sRange
    End Sub

    Private Sub Reset()
        Dur = rrr.Next(256, 512)
        MyEnd = rrr.NextDouble()
        MyInc = (MyEnd - CurVal) / Dur
    End Sub

    Public ReadOnly Property Value() As Double
        Get
            Dur -= 1.0
            If Dur < 0.0 Then
                Reset()
            End If
            Dim retval = CurVal
            CurVal += MyInc
            Return Base + (MyRange * retval)
        End Get
    End Property

End Class
