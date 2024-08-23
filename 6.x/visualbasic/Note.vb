Imports System.Text

Public Class Note
    Private m_Pfields As Double()

    Public Sub New(parms As Double())
        m_Pfields = parms
    End Sub

    Public Property Pfields As Double()
        Get
            Return m_Pfields
        End Get
        Private Set(value As Double())
            m_Pfields = value
        End Set
    End Property

    Public Overrides Function ToString() As String
        Dim note = New StringBuilder("i")
        note.Append(CInt(Pfields(0)))
        For i = 1 To Pfields.GetUpperBound(0)
            note.Append(" ")
            If Not (i = 4) Then
                note.Append(Pfields(i))
            Else
                note.Append(String.Format("{0:##}.{1:00}", CInt(3.0 + (Pfields(i) / 12.0)), Pfields(i) Mod 12))
            End If

        Next
        Return note.ToString()
    End Function
End Class
