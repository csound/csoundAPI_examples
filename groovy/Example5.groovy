/**
 * Example 5 - Generating Score Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * In this example, we will look at three techniques for generating our Score.
 *
 * The first is one we have already seen, which is to just write out the score
 * by hand as a String.
 *
 * Knowing that we pass strings into Csound to pass note events, we can also
 * generate the string. In the second example, sco2 starts as an empty
 * StringBuilder. Using a for-loop, we append to sco2 note strings using a
 * string formatting string that has its replacement values replaced. The
 * replace values are calculated using the i value, and the result is an
 * ascending note line.
 *
 * In the final example, we are going to generate a list of lists. The top-level
 * list represents our score as a whole, and each sub-list within it represents
 * the data for a single note. The main list is then processed in two ways:
 * first, it processes each sub-list and joins the values together into a single
 * note string second, it joins each individual note string into a single,
 * large score string, separated by newlines. The end result is a sequence of 13
 * notes with random pitches.
 *
 * The final example represents a common pattern of development. For systems
 * that employ some event-based model of music, it is common to use some kind of
 * data structure to represent events. This may use some kind of common data
 * structure like a list, or it may be represented by using a class and
 * instances of that class.
 *
 * Note, the three examples here are indicated with comments. To listen to the
 * examples, look for the lines that have c.ReadScore(sco) (lines 135-137),
 * uncomment the one you want to hear, and comment out the others.
 */
import csnd6.csnd6
import csnd6.Csound
import java.util.ArrayList
import java.util.List


def example1() {
    "i1 0 1 0.5 8.00"
}

def example2() {

    def sco2 = new StringBuilder()
    for (int i = 0 i < 13 i++) {
        sco2.append(
                String.format("i1 %g .25 0.5 8.%02d\n", i * 0.25, i))
    }

    sco2.toString()
}

def example3() {
    def vals = []

    // initialize a list to hold lists of values 
    for (i in 0..12) {
        def values = []
        values[0] = 1
        values[1] = i * 0.25
        values[2] = 0.25
        values[3] = 0.5
        values[4] = String.format("8.%02d", (int) (Math.random() * 15))
        vals.add(values)
    }

    // convert list of values into a single string
    StringBuilder buffer = new StringBuilder()
    for (list in vals) {
        buffer.append("i").append(list.collect{it.toString()}.join(' ')).append("\n")
    }

    buffer.toString()
}



csnd6.csoundInitialize(
        csnd6.CSOUNDINIT_NO_ATEXIT | csnd6.CSOUNDINIT_NO_SIGNAL_HANDLER)

// Defining our Csound ORC code within a String
def orc = """sr=44100
        ksmps=32
        nchnls=2
        0dbfs=1

        instr 1 
        ipch = cps2pch(p5, 12)
        kenv linsegr 0, .05, 1, .05, .7, .4, 0
        aout vco2 p4 * kenv, ipch
        aout moogladder aout, 2000, 0.25
        outs aout, aout
        endin"""

/* SCORE EXAMPLES */

// Example 1 - Static Score
def sco = example1()

// Example 2 - Generating Score string with a loop
def sco2 = example2()

// Example 3 - Generating Score using intermediate data structure (list of lists),
// then converting to String.
def sco3 = example3()

/* END SCORE EXAMPLES */

// Create an instance of the Csound object
Csound c = new Csound()

// Using SetOption() to configure Csound
// Note: use only one commandline flag at a time 
c.SetOption("-odac")

// Compile the Csound Orchestra string
c.CompileOrc(orc)

// Compile the Csound SCO String
//        c.ReadScore(sco)
//        c.ReadScore(sco2)
c.ReadScore(sco3)

// When compiling from strings, this call is necessary before doing 
// any performing
c.Start()

// The following is our main performance loop. We will perform one block 
// of sound at a time and continue to do so while it returns 0, which 
// signifies to keep processing.  We will explore this loop technique in 
// further examples.

while (c.PerformKsmps() == 0) {
    // pass for now
}


// stops Csound
c.Stop()

// clean up Csound this is useful if you're going to reuse a Csound 
// instance
c.Cleanup()

