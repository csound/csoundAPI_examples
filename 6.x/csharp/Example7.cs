using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using csound6netlib;  //Exposes DotNet to Csound 6 Bridge's classes from csound6netlib.dll

namespace csoundAPI_examples
{
    public partial class CsoundAPI_Examples
    {
        /* Example 7 - Communicating continuous values with Csound's Channel System
        * 
        * This example introduces using Csound's Channel System to communicate 
        * continuous control data (k-rate) from a host program to Csound. The 
        * first thing to note is the RandomLine class. It takes in a base value
        * and a range in which to vary randomly.  The reset functions calculates
        * a new random target value (this.End), a random duration in which to 
        * run (this.Dur, expressed as # of audio blocks to last in duration), and
        * calculates the increment value to apply to the current value per audio-block.
        * When the target is met, the Randomline will reset itself to a new target
        * value and duration.
        *
        * In this example, we use two RandomLine objects, one for amplitude and 
        * another for frequency.  We start a Csound instrument instance that reads
        * from two channels using the chnget opcode. In turn, we update the values
        * to the channel from the host program.  In this case, because we want to 
        * keep our values generating in sync with the audio engine, we use a 
        * while-loop instead of a CsoundPerformanceThread. To update the channel,
        * we call the SoftwareBus's channel indexer methods with new values.
        * 
        * Note: The Value property on the RandomLine objects not only gets
        * us the current value, but also advances the internal state by the increment
        * and by decrementing the duration.
        * 
        * In the C# object wrappers, channels are objects which are most easily used
        * via a software bus object as this implemetation demonstrates.
        */
        public void Example7()
        {
            using (var c = new Csound6NetRealtime())
            {
                c.SetOption("-odac");   // Set option for Csound
                c.SetOption("-m7");     // Set option for Csound
                c.CompileOrc(orc3);     // Compile Orchestra from String
                c.ReadScore("i1 0 10\n");//Read in a score to run for 10 seconds 
                c.Start();              //Must call Start() explicitly when compiling from a string

                var amp = new RandomLine(.4, .2);   // create RandomLine for use with Amplitude
                var freq = new RandomLine(400, 80); // create RandomLine for use with Frequency 

                var bus = c.GetSoftwareBus();
                //Channels can be created explicitly:
                bus.AddControlChannel("amp", ChannelDirection.Input);
                bus["amp"] = amp.Value;  //Add an initial value: Value property changes after each use

                //Or channels can be created implicitly just by using it:
                //The bus's channels can be accessed by name like a dictionary.
                //If they don't yet exist, they will be defined (input/output as default)
                bus["freq"] = freq.Value;//Create and provide and initial value in one call

                //Now we have our channels: update them with new RandomLine values after each ksmps cycle:
                while (!c.PerformKsmps())
                {
                    bus["amp"] = amp.Value;  
                    bus["freq"] = freq.Value;
                }
                c.Stop();
            }
        }

    }
}
