using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using csound6netlib;

namespace csoundAPI_examples
{
    /**
     * Example 9 - More efficient Channel Communications
     *           - Also, using the CSOUND_PARAMS structure (Csound6Parameters object in C#)
     *           
     * This example in the python examples refactors Example 8 to encapsulate channel
     * access into an object of its own.  Since the C# object wrappers already do this,
     * this example remains essentially the same as Example 8.
     * 
     * We use Example 9 in C# to explore the Csound6Parameters object as an alternative
     * way to set csound parameters when neither the command line nor CsOptions section
     * of a CSD file is used - as is the case in all of these examples.
     */
    public partial class CsoundAPI_Examples
    {

        public void Example9()
        {
            using (var c = new Csound6NetRealtime())
            {
                c.SetOutputDac(0);  //direct sample output to sound card

                //SetOptions requires the knowing and using the command line flags and their arguments.
                //The csound API (and C# bridge) expose another way to set csounds
                //parameters as properties.  This would suit a GUI dialog better than SetOption.
                var options = c.GetParameters(); //also var options = new Csound6Parameters(c);

                //You can read their current value: Console.Write(options.Tempo);
                Console.WriteLine(string.Format("Current Message Level = {0}", options.MessageLevel));

                //Instead of SetOptions("-m7"), you can use:
                options.MessageLevel = MessageLevel.Amps | MessageLevel.Range | MessageLevel.Warnings; //=7

                //Using IntelliSense, you can explore the various settings csound exposes (various command line flags).
                options.IsDebugMode = false;
                options.Tempo = 120; //will cause 10 in score to be 5 (tempo defaults to 60)

                c.CompileOrc(orc3);     // Compile Orchestra from String
                c.ReadScore("i1 0 10\n");//Read in a score to run for 10 seconds 
                c.Start();              //Must call Start() explicitly when compiling from a string

                var amps = new RandomLine(.4, .2);   // create RandomLine for use with Amplitude
                var freqs = new RandomLine(400, 80); // create RandomLine for use with Frequency 

                var bus = c.GetSoftwareBus();//Get bus and define the "amp" and "freq" channels of orc3
                //Since orc3 instrument doesn't declare channels directly, define them here
                var ampChannel = bus.AddControlChannel("amp", ChannelDirection.Input);
                var freqChannel = bus.AddControlChannel("freq", ChannelDirection.Input);

                //Prime with initial values accessing channel memory directly
                //That is, it contains a call to csoundGetChannelPtr internally.
                ampChannel.SetValueDirect(amps.Value); //Use 
                freqChannel.SetValueDirect(freqs.Value);

                while (!c.PerformKsmps())
                {
                    //continue to update memory for the two channels directly
                    ampChannel.SetValueDirect(amps.Value);
                    freqChannel.SetValueDirect(freqs.Value);
                }

                c.Stop();

            }
        }
    }
}
