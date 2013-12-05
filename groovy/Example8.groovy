/** Example 8 - More efficient Channel Communications
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example builds on Example 7 by replacing the calls to SetChannel
 * with using GetChannelPtr. In the Csound API, using SetChannel and GetChannel
 * is great for quick work, but ultimately it is slower than pre-fetching the
 * actual channel pointer.  This is because Set/GetChannel operates by doing a 
 * lookup of the Channel Pointer, then setting or getting the value.  This 
 * happens on each call. The alternative is to use GetChannelPtr, which fetches
 * the Channel Pointer and lets you directly set and get the value on the pointer.
 *
 * In C/C++/Objective-C, one can directly use MYFLT* to get/set values.  However,
 * for wrapped languages such as Python, Java, and Lua, it is generally not possible
 * to get/set the value on the pointer itself.  The Csound API for host languages 
 * uses a special wrapper object called CsoundMYFLTArray, which will hold a reference
 * to a MYFLT*.  The CsoundMYFLTArray in turn has convenience methods for setting
 * and getting values. 
 *
 * The code below shows how to use the CsoundMYFLTArray in conjunction with GetChannelPtr
 * to have a more optimized channel setting system.
 */


import csnd6.csnd6
import csnd6.Csound
import csnd6.CsoundMYFLTArray
import csnd6.controlChannelType


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


// create a CsoundMYFLTArray of size 1 
def ampChannel = new CsoundMYFLTArray(1)
def freqChannel = new CsoundMYFLTArray(1)  

// the following calls store the Channel Pointer retreived from Csound 
// into the CsoundMYFLTArray Objects
c.GetChannelPtr(ampChannel.GetPtr(), "amp", 
        controlChannelType.CSOUND_CONTROL_CHANNEL.swigValue() | 
        controlChannelType.CSOUND_INPUT_CHANNEL.swigValue()) 
c.GetChannelPtr(freqChannel.GetPtr(), "freq", 
        controlChannelType.CSOUND_CONTROL_CHANNEL.swigValue() | 
        controlChannelType.CSOUND_INPUT_CHANNEL.swigValue()) 

// note we are now setting values on the CsoundMYFLTArray
ampChannel.SetValue(0, amp.getValue())
freqChannel.SetValue(0, freq.getValue())

// The following is our main performance loop. We will perform one block 
// of sound at a time and continue to do so while it returns 0, which 
// signifies to keep processing.  

while (c.PerformKsmps() == 0) {
    // Update channel values
    ampChannel.SetValue(0, amp.getValue())
    freqChannel.SetValue(0, freq.getValue())
}

// stops Csound
c.Stop()

// clean up Csound this is useful if you're going to reuse a Csound 
// instance
c.Cleanup()

