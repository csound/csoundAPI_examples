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


import csnd6.Csound
import csnd6.CsoundMYFLTArray
import csnd6.controlChannelType
import csnd6.csnd6

class RandomLine {
    double curVal = 0.0
    double base
    double range
    int dur
    double end
    double increment

    public RandomLine(double base, double range) {
        this.base = base
        this.range = range
        this.reset()
    }

    def reset() {
        dur = 256 + (int)(Math.random() * 256)
        end = Math.random()
        increment = (end - curVal) / dur
    }

    public double getValue() {
        double retVal = curVal

        dur -= 1
        if(dur < 0) {
            reset()
        }

        curVal += increment
        base + (retVal * range)
    }
}

class ChannelUpdater {

    def updater
    def channel

    public ChannelUpdater(csound, channelName, updater) {
        this.updater = updater
        this.channel = createChannel(csound, channelName)
    }

    public void update() {
        channel.SetValue(0, updater.getValue())
    }
    

    /**
      * Creates a Csound Channel and returns a CsoundMYFLTArray wrapper
      * object
      */
    public CsoundMYFLTArray createChannel(Csound csound, String channelName) {
        def channel = new CsoundMYFLTArray(1)

        csound.GetChannelPtr(channel.GetPtr(), channelName,
                controlChannelType.CSOUND_CONTROL_CHANNEL.swigValue()
                | controlChannelType.CSOUND_INPUT_CHANNEL.swigValue())

        return channel
    }
}


csnd6.csoundInitialize(
        csnd6.CSOUNDINIT_NO_ATEXIT | csnd6.CSOUNDINIT_NO_SIGNAL_HANDLER)

// Defining our Csound ORC code within a String
def orc = """

sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
kamp chnget "amp"
kfreq chnget "freq"
kres chnget "resonance"
printk 0.5, kamp
printk 0.5, kfreq
printk 0.5, kres
aout vco2 kamp, kfreq
aout moogladder aout, 2000, kres 
outs aout, aout
endin

"""

/* SCORE EXAMPLES */

// Generate Score 
String sco = "i1 0 60\n"

/* END SCORE EXAMPLES */

// Create an instance of the Csound object
Csound c = new Csound()

// Using SetOption() to configure Csound
// Note: use only one commandline flag at a time 
c.SetOption("-odac")

// Compile the Csound Orchestra string
c.CompileOrc(orc)

// Compile the Csound SCO String
c.ReadScore(sco)

// When compiling from strings, this call is necessary before doing 
// any performing
c.Start()

// Create a set of ChannelUpdaters
def channels = [ 
    new ChannelUpdater(c, "amp", new RandomLine(0.4, 0.2)),
    new ChannelUpdater(c, "freq", new RandomLine(400, 80)),
    new ChannelUpdater(c, "resonance", new RandomLine(0.4, 0.3))
    ]

// Initialize all Channel Values
for (updater in channels) {
    updater.update()
}

// The following is our main performance loop. We will perform one block 
// of sound at a time and continue to do so while it returns 0, which 
// signifies to keep processing.  

while (c.PerformKsmps() == 0) {
    // Update channel values
    for (updater in channels) {
        updater.update()
    }
}

// stops Csound
c.Stop()

// clean up Csound this is useful if you're going to reuse a Csound 
// instance
c.Cleanup()
