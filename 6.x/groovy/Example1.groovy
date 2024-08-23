/**
 * Example 1 - Simple Compilation with Csound
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example is a barebones example for creating an instance of Csound, 
 * compiling a pre-existing CSD, calling Perform to run Csound to completion,
 * then Stop and exit.  
 *
 * The first thing we do is import the csnd6 module, which is the module 
 * containing the Python interface to the Csound API.
 *
 * @author stevenyi
 */
import csnd6.csnd6
import csnd6.Csound

csnd6.csoundInitialize(
        csnd6.CSOUNDINIT_NO_ATEXIT | csnd6.CSOUNDINIT_NO_SIGNAL_HANDLER)

// Create an instance of the Csound object
Csound c = new Csound()

// Compile a pre-defined test1.csd file
// This path should be modified for your own path on your computer
c.Compile("test1.csd")

// This call runs Csound to completion
c.Perform()               

// At this point, Csound is already stopped, but this call is here
// as it is something that you would generally call in real-world 
// contexts
c.Stop()                                                               

