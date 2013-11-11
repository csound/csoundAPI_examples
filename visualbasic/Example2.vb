Imports csound6netlib   'Exposes DotNet to Csound 6 Bridge's classes from csound6netlib.dll

Partial Public Class Examples

    ' Example 2 - Compilation with Csound without CSD

    ' In this example, we move from using an external CSD file to 
    ' embedding our Csound ORC and SCO code directly in our VB project.
    ' Besides allowing encapsulation of the code within the same file,
    ' using the CompileOrc() and CompileSco() API calls is useful when
    ' the SCO or ORC are generated, or perhaps coming from another 
    ' source, such as from a database or network.

    Public Sub Example2()

        Using c As New Csound6Net

            'Using SetOption() to configure Csound: here to output in realtime
            c.SetOption("-odac")    'Note: SetOption uses only one commandline flag per call

            c.CompileOrc(orc)       'Compile the Csound Orchestra string
            c.ReadScore("i1 0 1\n") ' Compile the Csound score as a string constant

            c.Start()   'When compiling from strings, Start() is needed before performing
            c.Perform() 'Run Csound to completion
            c.Stop()    'At this point, Csound is already stopped, but this call is here
            'as it is something that you would generally call in real-world contexts.

        End Using

    End Sub

    'Used in examples 2, 3 and 4
    Public Const orc = "sr=44100" & vbCrLf & _
                        "ksmps=32" & vbCrLf & _
                        "nchnls=2" & vbCrLf & _
                        "0dbfs=1" & vbCrLf & _
                        "instr 1 " & vbCrLf & _
                        "aout vco2 0.5, 440" & vbCrLf & _
                        "outs aout, aout" & vbCrLf & _
                        "endin"

End Class
