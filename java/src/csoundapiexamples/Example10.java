package csoundapiexamples;

import csnd6.Csound;
import csnd6.CsoundMYFLTArray;
import csnd6.controlChannelType;
import csnd6.csnd6;

/**
 * Example 10 - More efficient Channel Communications Author: Steven Yi
 * <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example continues on from Example 10 and introduces a ChannelUpdater
 * object. The ChannelUpdater will create and store a CsoundMYFLTArray that is
 * wrapping a Csound Channel. Additionally, it will store and call an Updater 
 * that has a getValue() method to update values in the channel when update() is
 * called.
 *
 * This example continues the illustration of a progression of a project. Note
 * that the process has changed a little bit where we now create a number of
 * ChannelUpdater objects and store them in a list. The list is then iterated
 * through for updating the channel with the latest values. In a real-world
 * project, this kind of scenario occurs when there are n-number of items to
 * update channels and one wants to have a flexible number that may even change
 * dynamically at runtime.
 */
public class Example10 {

    public static interface Updater {

        public double getValue();
    }

    public static class RandomLine implements Updater {

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

    public static class ChannelUpdater {

        private final Updater updater;
        private final CsoundMYFLTArray channel;

        public ChannelUpdater(Csound csound, String channelName, Updater updater) {
            this.updater = updater;
            this.channel = createChannel(csound, channelName);
        }

        public void update() {
            channel.SetValue(0, updater.getValue());
        }

        /**
         * Creates a Csound Channel and returns a CsoundMYFLTArray wrapper
         * object
         */
        public CsoundMYFLTArray createChannel(Csound csound, String channelName) {
            CsoundMYFLTArray channel = new CsoundMYFLTArray(1);

            csound.GetChannelPtr(channel.GetPtr(), channelName,
                    controlChannelType.CSOUND_CONTROL_CHANNEL.swigValue()
                    | controlChannelType.CSOUND_INPUT_CHANNEL.swigValue());

            return channel;
        }
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
                + "kres chnget \"resonance\"\n"
                + "printk 0.5, kamp\n"
                + "printk 0.5, kfreq\n"
                + "printk 0.5, kres\n"
                + "aout vco2 kamp, kfreq\n"
                + "aout moogladder aout, 2000, kres\n"
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

        // Create a set of ChannelUpdaters
        ChannelUpdater[] channels = {
            new ChannelUpdater(c, "amp", new RandomLine(.4, .2)),
            new ChannelUpdater(c, "freq", new RandomLine(400, 80)),
            new ChannelUpdater(c, "resonance", new RandomLine(0.4, .3))};

        // Initialize all Channel Values
        for (ChannelUpdater updater : channels) {
            updater.update();
        }

        // The following is our main performance loop. We will perform one block 
        // of sound at a time and continue to do so while it returns 0, which 
        // signifies to keep processing.  

        while (c.PerformKsmps() == 0) {
            // Update channel values
            for (ChannelUpdater updater : channels) {
                updater.update();
            }
        }

        // stops Csound
        c.Stop();

        // clean up Csound; this is useful if you're going to reuse a Csound 
        // instance
        c.Cleanup();

    }
}
