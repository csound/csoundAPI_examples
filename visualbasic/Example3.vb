Imports csound6netlib

Partial Public Class Examples

    'Example 3 - Using our own performance loop

    ' In this example, we use a while loop to perform Csound one audio block at a time.
    ' This technique is important to know as it will allow us to do further processing
    ' safely at block boundaries.  We will explore the technique further in later examples.

    ' The VB version includes checking the status code which csound returns after key processing.
    ' These codes are encapsulated in the CsoundStatus enumeration.
    ' This demonstrates the traditional way to respond to csound compiling and performing

    Public Sub Example3()
        Dim sco = "i1 0 1" & vbCrLf

        Using c As New Csound6Net

            'You can also set the output file or device using SetOutputFileName method in the API
            'SetOutputFileName(string path, SoundFileType type, SampleFormat format)
            'or its convenience method for real time output SetOutputDac(int dacNbr)
            'instead of via command line arguments fed to SetOption(string option) one at a time

            c.SetOutputDac(0)

            Dim status = c.CompileOrc(orc)          'Compile Orchestra from String
            'Classic csound practice tests compile results before proceeding 

            If status = CsoundStatus.Success Then

                status = c.ReadScore(sco)           'Read in Score from String variable
                If (status = CsoundStatus.Success) Then

                    c.Start() 'When compiling from strings, this call needed before performing

                    ' The following is our main performance loop.
                    ' We will perform one block of sound at a time and continue to do so
                    ' while it returns false, which tells us to keep processing.
                    ' We will explore this loop technique in further examples.

                    While c.PerformKsmps() = False
                    End While

                    c.Stop()
                End If
            End If

        End Using

    End Sub

End Class
