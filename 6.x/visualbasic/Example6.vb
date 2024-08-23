Imports System.Text
Imports csound6netlib

Partial Public Class Examples

    'Example 6 - Generating a Score

    ' This example continues on from Example 5, rewriting the example using
    ' a Class called Note (in another class file for better reuse).
    ' The note example has its ToString() method implemented
    ' to generate a well-formatted Csound SCO note.  

    ' This example also shows how a list of notes could be used multiple times.
    ' The first loop through we use the notes as-is, and during the second time
    ' we generate the notes again with the same properties except we alter the 
    ' fifth p-field up 4 semitones. 

    ' Note: Altering a Notes values like this is alright for this example, but 
    ' it is a destructive edit.  Real world code might make copies of Notes or 
    ' alter the score generation to maintain the original values. 

    Public Sub Example6()

        Dim r As New Random

        'Create score from a generated list of Note objects using Midi pitches 60-75.
        Dim notes = New List(Of Note)
        For i = 0 To 12  ' P1  P2-start P3-Dur P4-amp   P5-frq 
            notes.Add(New Note(New Double() {1, i * 0.25, 0.25, 0.5, r.Next(60, 75)}))
        Next i

        'Capture the note values as Csound parameters in a .net System.Text.StringBuilder
        Dim sco = New StringBuilder
        For Each n As Note In notes
            sco.AppendLine(n.ToString()) 'note.ToString() will look like a score statement    
        Next n

        'Add a major third and time displacement to the Note objects and put them in the score as well.
        For Each n As Note In notes
            If n.Pfields.Length > 4 Then
                n.Pfields(4) += 4 'pitch (p5)
            End If
            If n.Pfields.Length > 1 Then
                n.Pfields(1) += 0.125 'start time (p2)
            End If
            sco.AppendLine(n.ToString()) 'More score statements with new freq and start
        Next n

        'Now that the score has been generated, apply it to Csound:
        Using c As New Csound6Net

            c.SetOption("-odac")  ' Set option for Csound

            Try
                c.CompileOrc(orc2)         'Compile the matching orchestra
                c.ReadScore(sco.ToString()) '//and apply the generated score

                c.Start()
                While c.PerformKsmps() = False
                End While

                c.Stop()

            Catch ex As Csound6NetException
                Console.WriteLine(ex.Message)
            End Try

        End Using

    End Sub

End Class
