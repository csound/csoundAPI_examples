Imports csound6netlib


Partial Public Class Examples

    ' Example 7 - Communicating continuous values with Csound's Channel System

    ' This example introduces using Csound's Channel System to communicate 
    ' continuous control data (k-rate) from a host program to Csound. The 
    ' first thing to note is the RandomLine class. It takes in a base value
    ' and a range in which to vary randomly.  The reset functions calculates
    ' a new random target value (Me.MyEnd), a random duration in which to 
    ' run (Me.Dur, expressed as # of audio blocks to last in duration), and
    ' calculates the increment value to apply to the current value per audio-block.
    ' When the target is met, the Randomline will reset itself to a new target
    ' value and duration.

    ' In this example, we use two RandomLine objects, one for amplitude and 
    ' another for frequency.  We start a Csound instrument instance that reads
    ' from two channels using the chnget opcode. In turn, we update the values
    ' to the channel from the host program.  In this case, because we want to 
    ' keep our values generating in sync with the audio engine, we use a 
    ' while-loop instead of a CsoundPerformanceThread. To update the channel,
    ' we call the SoftwareBus's channel indexer methods with new values.
    ' 
    ' Note: The Value property on the RandomLine objects not only gets
    ' us the current value, but also advances the internal state by the increment
    ' and by decrementing the duration.

    ' In the VB object wrappers, channels are objects which are most easily used
    ' via a software bus object as this implemetation demonstrates.


    Public Sub Example7()
        Dim amp = New RandomLine(0.4, 0.2) ' create RandomLine for use with Amplitude
        Dim freq = New RandomLine(400, 80) ' create RandomLine for use with Frequency 
        Using c As New Csound6NetRealtime

            Try
                c.SetOption("-odac")    ' Set option for Csound
                c.SetOption("-m7")      ' Set option for Csound
                c.CompileOrc(orc3)      ' Compile Orchestra from String
                c.ReadScore("i1 0 10")  'Read in a score to run for 10 seconds 
                c.Start()          'Must call Start() explicitly when compiling from a string

                Dim bus = c.GetSoftwareBus
                'Channels can be created explicitly:
                bus.AddControlChannel("amp", ChannelDirection.Input)
                bus("amp") = amp.Value  'Add an initial value: Value property changes after each use

                'Or channels can be created implicitly just by using it:
                'The bus's channels can be accessed by name like a dictionary.
                '/If they don't yet exist, they will be defined (input/output as default)
                bus("freq") = freq.Value  'Create and provide and initial value in one call

                'Now we have our channels: update them with new RandomLine values after each ksmps cycle:
                While c.PerformKsmps = False
                    bus("amp") = amp.Value
                    bus("freq") = freq.Value
                End While

                c.Stop()

            Catch ex As Exception
                Console.WriteLine(ex.Message)
            End Try

        End Using

    End Sub

    'Used in examples 7,8 and 9
    Public Const orc3 = "sr=44100" & vbCrLf & _
                        "ksmps=32" & vbCrLf & _
                        "nchnls=2" & vbCrLf & _
                        "0dbfs=1" & vbCrLf & _
                        "instr 1" & vbCrLf & _
                        "kamp chnget ""amp""" & vbCrLf & _
                        "kfreq chnget ""freq""" & vbCrLf & _
                        "printk 0.5, kamp" & vbCrLf & _
                        "printk 0.5, kfreq" & vbCrLf & _
                        "aout vco2 kamp, kfreq" & vbCrLf & _
                        "aout moogladder aout, 2000, 0.25" & vbCrLf & _
                        "outs aout, aout" & vbCrLf & _
                        "endin"

End Class
