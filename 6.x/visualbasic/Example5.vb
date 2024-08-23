Imports System.Text
Imports System.Collections.Generic
Imports csound6netlib

Partial Public Class Examples

    ' Example 5 - Generating a Score, VB Try/Catch blocks

    ' In this example, we will look at three techniques for generating our Score. 
    ' The first is one we have already seen, which is to just write out the score
    ' by hand as a String.

    ' Knowing that we pass strings into Csound to pass note events, we can also
    ' generate the string.  In the second example, the score starts as an empty string.
    ' Using a for-loop, we append to sco note strings using a string formatting
    ' string that has its replacement values replaced.  The replace values are 
    ' calculated using the i value, and the result is an ascending note line. 

    ' In the final example, we are going to generate a list of lists.  The top-level
    ' list represents our score as a whole, and each sub-list within it represents
    ' the data for a single note.  The main list is then processed in two ways: first,
    ' it processes each sub-list and joins the values together into a single note string;
    ' second, it joins each individual note string into a single, large score string,
    ' separated by newlines.  The end result is a sequence of 13 notes with random
    ' pitches.

    ' The final example represents a common pattern of development.  For systems that
    ' employ some event-based model of music, it is common to use some kind of data
    ' structure to represent events.  This may use some kind of common data structure
    ' like a list, or it may be represented by using a class and instances of that
    ' class. 

    ' Note, the three examples here are indicated within case statement. To listen to the examples,
    ' use a real number when selecting Example 5: 5.0 for the single note, 5.1 for the
    ' chromatic scale and 5.2 for random pitches.

    Public Sub Example5(algorithm As Integer)

        Dim sco As New StringBuilder


        'Choose one of 3 ways to create a score: sequential pitches, random pitches, or hard-coded
        Select Case algorithm 'integer between 0 and 2
            Case 1
                For i = 0 To 12
                    sco.AppendLine(String.Format("i1 {0} .25 0.5 8.{1:00}", (i * 0.25), i))
                Next i
            Case 2
                Dim vals = New List(Of Double()) 'initialize a list to hold lists of doubles 
                Dim r = New Random()
                For i = 0 To 12     'populate that list with note parameters (pitch is random number from 0 to 14
                    vals.Add(New Double() {i * 0.25, 0.25, 0.5, r.Next(15)})
                Next i
                ' convert list of lists into a list of strings with random frequencys
                For Each val As Double() In vals
                    sco.AppendLine(String.Format("i1 {0} {1} {2} 8.{3:00}", val(0), val(1), val(2), CInt(val(3))))
                Next val
            Case Else
                sco.AppendLine("i1 0 1 0.5 8.00")
        End Select

        ' Once a score string has been algorithmically assembled, do the usual csound loop.
        ' This time, we'll let C# Exceptions (try/catch) handle testing status for us.
        ' By catching the exception, we are assured exiting the "using" block
        ' and thereby disposing of csound and its memory correctly.

        Using c As New Csound6Net

            c.SetOutputDac(0)   ' Set realtime output 

            Try
                c.CompileOrc(orc2)          ' Compile different Orchestra with moogladder 
                c.ReadScore(sco.ToString()) ' Read in score as built above 
                c.Start()                   ' When compiling from strings, this call needed before performing

                While c.PerformKsmps() = False 'Play score until csound says it is over
                End While

                c.Stop()
            Catch ex As Csound6NetException
                Console.WriteLine(ex.Message)
            End Try
        End Using

    End Sub


    'Used in examples 5 and 6
    Public Const orc2 = "sr=44100" & vbCrLf & _
                        "ksmps=32" & vbCrLf & _
                        "nchnls=2" & vbCrLf & _
                        "0dbfs=1" & vbCrLf & _
                        "instr 1 " & vbCrLf & _
                        "ipch = cps2pch(p5, 12)" & vbCrLf & _
                        "kenv linsegr 0, .05, 1, .05, .7, .4, 0" & vbCrLf & _
                        "aout vco2 p4 * kenv, ipch" & vbCrLf & _
                        "aout moogladder aout, 2000, 0.25" & vbCrLf & _
                        "outs aout, aout" & vbCrLf & _
                        "endin"

End Class
