package csoundapiexamples;

/**
 * Example 3 - Using our own performance loop 
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * In this example, we use a while loop to perform Csound one audio block at a
 * time. This technique is important to know as it will allow us to do further
 * processing safely at block boundaries. We will explore the technique further
 * in later examples.
 *
 * @author stevenyi
 */

import csnd6.csnd6;
import csnd6.Csound;

public class Example3 {

    public static void main(String[] args) {
        csnd6.csoundInitialize(
                csnd6.CSOUNDINIT_NO_ATEXIT | csnd6.CSOUNDINIT_NO_SIGNAL_HANDLER);

        // Defining our Csound ORC code within a String
        String orc = "sr = 44100\n"
                + "ksmps = 32\n"
                + "nchnls = 2\n"
                + "0dbfs = 1\n\n"
                + "instr 1\n"
                + "aout vco2 0.5, 440\n"
                + "outs aout, aout\n"
                + "endin";

        // Defining our Csound SCO code 
        String sco = "i1 0 1";

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

        // The following is our main performance loop. We will perform one block 
        // of sound at a time and continue to do so while it returns 0, which 
        // signifies to keep processing.  We will explore this loop technique in 
        // further examples.

        while (c.PerformKsmps() == 0) {
            // pass for now
        }

        c.Stop();

    }
}
