package csoundapiexamples;

import csnd6.Csound;
import csnd6.CsoundMYFLTArray;
import csnd6.controlChannelType;
import csnd6.csnd6;

/**
 * Example 9 - More efficient Channel Communications Author: Steven Yi
 * <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example continues on from Example 9 and just refactors the creation and
 * setup of CsoundMYFLTArray's into a createChannel() function. This example
 * illustrates some natural progression that might occur in your own API-based
 * projects, and how you might simplify your own code.
 */
public class Example9 {

    public static class RandomLine {

        double curVal = 0.0;
        double base;
        double range;
        int dur;
        double end;
        double increment;

        public RandomLine(double base, double range) {
            this.base = base;
            this.range = range;
            this.reset();
        }

        public void reset() {
            dur = 256 + (int) (Math.random() * 256);
            end = Math.random();
            increment = (end - curVal) / dur;
        }

        public double getValue() {
            double retVal = curVal;

            dur -= 1;
            if (dur < 0) {
                reset();
            }

            curVal += increment;
            return base + (retVal * range);
        }
    }

    /**
     * Creates a Csound Channel and returns a CsoundMYFLTArray wrapper object
     */
    public static CsoundMYFLTArray createChannel(Csound csound, String channelName) {
        CsoundMYFLTArray channel = new CsoundMYFLTArray(1);

        csound.GetChannelPtr(channel.GetPtr(), channelName,
                controlChannelType.CSOUND_CONTROL_CHANNEL.swigValue()
                | controlChannelType.CSOUND_INPUT_CHANNEL.swigValue());

        return channel;
    }

    public static void main(String[] args) {
        csnd6.csoundInitialize(
                csnd6.CSOUNDINIT_NO_ATEXIT | csnd6.CSOUNDINIT_NO_SIGNAL_HANDLER);

        // Defining our Csound ORC code within a String
        String orc = "sr=44100\n"
                + "ksmps=32\n"
                + "nchnls=2\n"
                + "0dbfs=1\n"
                + "\n"
                + "instr 1 \n"
                + "kamp chnget \"amp\"\n"
                + "kfreq chnget \"freq\"\n"
                + "printk 0.5, kamp\n"
                + "printk 0.5, kfreq\n"
                + "aout vco2 kamp, kfreq\n"
                + "aout moogladder aout, 2000, 0.25\n"
                + "outs aout, aout\n"
                + "endin";

        /* SCORE EXAMPLES */

        // Generate Score 
        String sco = "i1 0 60\n";

        /* END SCORE EXAMPLES */

        // Create an instance of the Csound object
        Csound c = new Csound();

        // Using SetOption() to configure Csound
        // Note: use only one commandline flag at a time 
        c.SetOption("-odac");

        // Compile the Csound Orchestra string
        c.CompileOrc(orc);

        // Compile the Csound SCO String
        c.ReadScore(sco);

        // When compiling from strings, this call is necessary before doing 
        // any performing
        c.Start();


        // create RandomLine for use with Amplitude and Frequency
        RandomLine amp = new RandomLine(.4, .2);
        RandomLine freq = new RandomLine(400, 80);


        // uses utility method to create a channel and get a CsoundMYFLTArray
        CsoundMYFLTArray ampChannel = createChannel(c, "amp");
        CsoundMYFLTArray freqChannel = createChannel(c, "freq");


        // note we are now setting values on the CsoundMYFLTArray
        ampChannel.SetValue(0, amp.getValue());
        freqChannel.SetValue(0, freq.getValue());

        // The following is our main performance loop. We will perform one block 
        // of sound at a time and continue to do so while it returns 0, which 
        // signifies to keep processing.  

        while (c.PerformKsmps() == 0) {
            // Update channel values
            ampChannel.SetValue(0, amp.getValue());
            freqChannel.SetValue(0, freq.getValue());
        }

        // stops Csound
        c.Stop();

        // clean up Csound; this is useful if you're going to reuse a Csound 
        // instance
        c.Cleanup();

    }
}
