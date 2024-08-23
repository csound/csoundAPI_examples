/**
 * Example 7 - Communicating continuous values with Csound's Channel System
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example introduces using Csound's Channel System to communicate
 * continuous control data (k-rate) from a host program to Csound. The first
 * thing to note is the RandomLine class. It takes in a base value and a range
 * in which to vary randomly. The reset functions calculates a new random target
 * value (self.end), a random duration in which to run (self.dur, expressed as #
 * of audio blocks to last in duration), and calculates the increment value to
 * apply to the current value per audio-block. When the target is met, the
 * Randomline will reset itself to a new target value and duration.
 *
 * In this example, we use two RandomLine objects, one for amplitude and another
 * for frequency. We start a Csound instrument instance that reads from two
 * channels using the chnget opcode. In turn, we update the values to the
 * channel from the host program. In this case, because we want to keep our
 * values generating in sync with the audio engine, we use a while-loop instead
 * of a CsoundPerformanceThread. To update the channel, we call the SetChannel
 * method on the Csound object, passing a channel name and value. Note: The
 * getValue method on the RandomLine not only gets us the current value, but
 * also advances the internal state by the increment and by decrementing the
 * duration.
 */

import csnd6.csnd6
import csnd6.Csound

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

    def getValue() {
        double retVal = curVal

        dur -= 1
        if(dur < 0) {
            reset()
        }

        curVal += increment
        base + (retVal * range)
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
printk 0.5, kamp
printk 0.5, kfreq
aout vco2 kamp, kfreq
aout moogladder aout, 2000, 0.25
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


// create RandomLine for use with Amplitude and Frequency
def amp = new RandomLine(0.4, 0.2)            
def freq = new RandomLine(400, 80) 

// Initialize channel values before running Csound
c.SetChannel("amp", amp.getValue())
c.SetChannel("freq", freq.getValue())

// The following is our main performance loop. We will perform one block 
// of sound at a time and continue to do so while it returns 0, which 
// signifies to keep processing.  

while (c.PerformKsmps() == 0) {
    // Update channel values
    c.SetChannel("amp", amp.getValue())
    c.SetChannel("freq", freq.getValue())
}

// stops Csound
c.Stop()

// clean up Csound this is useful if you're going to reuse a Csound 
// instance
c.Cleanup()


