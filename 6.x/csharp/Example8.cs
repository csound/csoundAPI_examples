using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using csound6netlib;

namespace csoundAPI_examples
{
    public partial class CsoundAPI_Examples
    {
        /**
         * Example 8 - More efficient Channel Communications (in a single threaded host)
         *
         * This example builds on Example 7 by replacing the calls to SetChannel
         * with using GetChannelPtr. In the Csound API, using SetChannel and GetChannel
         * is great for quick work, but ultimately it is slower than pre-fetching the
         * actual channel pointer.  This is because Set/GetChannel operates by doing a 
         * lookup of the Channel Pointer, then setting or getting the value.  This 
         * happens on each call. The alternative is to use GetChannelPtr, which fetches
         * the Channel Pointer and lets you directly set and get the value on the pointer.
         * The approach shown here is only safe if you remain in a single thread
         * such as managing channel values in between calls to PerformKsmps.
         * 
         * The thread-safe method shown in Example6 is preferred when using events in 
         * a multithreaded environment such as was hinted at in Example4.
         * You can use the Channel Pointer technique when using threads but you must use
         * a channel lock to take care in not to creating a race condition while using
         * memory shared between host and csound itself thereby causing potential glitches
         * in your music.  Further, since you write directly into unmanaged memory when
         * using this technique, getting even one byte of data wrong can crash your program.
         * 
         * Like other managed hosts (like python), C# provides mechanisms that attempt
         * to contain your ability to do this.
         * The C# version supplies Get/SetValueDirect methods for channels which use
         * the channel objects declared name and direction and limits memory access to the
         * actual size defined for the channel's type.
         * The call to the memory pointer (GetChannelPtr) is internal and the returned pointer
         * is used whenever GetValueDirect or SetValueDirect is called thereby preserving
         * the speed advantage mentioned in the python example.
         * 
         * Managing thread locks in host code is beyond the scope of this tutorial.
         * Use the method shown in Example6 when using more than one thread.
         * 
         * The code below shows how to make indirect use of the .net Csound6Channel
         * object's GetChannelPtr function to touch channel memory directly.
         *
         */

        public void Example8()
        {
            using (var c = new Csound6NetRealtime())
            {
                c.SetOutputDac(0);
                c.SetOption("-m7");
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
