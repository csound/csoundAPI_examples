/**
 * Example 2 - Compilation with Csound without CSD 
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * In this example, we move from using an external CSD file to embedding our
 * Csound ORC and SCO code within our Python project. Besides allowing
 * encapsulating the code within the same file, using the CompileOrc() and
 * CompileSco() API calls is useful when the SCO or ORC are generated, or
 * perhaps coming from another source, such as from a database or network.
 *
 * @author stevenyi
 */
import csnd6.csnd6
import csnd6.Csound

csnd6.csoundInitialize(
        csnd6.CSOUNDINIT_NO_ATEXIT | csnd6.CSOUNDINIT_NO_SIGNAL_HANDLER)

// Defining our Csound ORC code within a String
String orc = """
sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1

instr 1
aout vco2 0.5, 440
outs aout, aout
endin
"""

// Defining our Csound SCO code 
String sco = "i1 0 1"

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

// This call runs Csound to completion
c.Perform()

c.Stop()

