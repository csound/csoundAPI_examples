Imports System.Threading
Imports csound6netlib


Partial Public Class Examples

    Public Sub Example4()
        Dim c As New Csound6NetRealtime

        Try
            c.SetOutputDac(0)      'Set realtime output for Csound
            c.CompileOrc(orc)      ' Compile Orchestra from String
            c.ReadScore("i1 0 1")  ' Read in Score from String

            c.Start()           'When compiling from strings, this call needed before performing

            ' Create a new CsoundPerformanceThread, passing in the Csound object
            Dim t = New Csound6PerformanceThread(c)

            t.Play()   'Starts the thread, which is now running separately from the main thread. This 
            '           call is asynchronous and will immediately return back here to continue code
            '           execution.
            t.Join()   'Join will wait for the other thread to complete. If we did not call Join(),
            '           after t.Play() returns we would immediate move to the next line, c.Stop(). 
            '           That would stop Csound without really giving it time to run. 
            c.Stop()   'stops Csound
            c.Cleanup() 'clean up Csound; this is useful if you're going to reuse a Csound instance

        Catch ex As Csound6NetException
            Console.WriteLine(ex.Message)
        Finally
            c.Dispose()
        End Try
    End Sub


    Public Async Function Example41() As Task


        Using c As New Csound6Net
            c.SetOption("-odac")  ' Set option for Csound
            c.CompileOrc(orc)     ' Compile Orchestra from String
            c.ReadScore("i1 0 1") ' Read in Score from String

            c.Start()      ' When compiling from strings, this call needed before performing

            'Perform the score in a background task until done
            'Neither a progress dialog or cancellation event is used in this call

            Await c.PerformAsync(Nothing, CancellationToken.None) 'handles cleanup on exit

        End Using

    End Function

End Class
