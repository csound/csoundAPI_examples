package csoundapiexamples;

/**
 * Example 6 - Generating Score 
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example continues on from Example 5, rewriting the example using a Class
 * called Note. The note example has its toString() method implemented to
 * generate a well-formatted Csound SCO note.
 *
 * This example also shows how a list of notes could be used multiple times. The
 * first loop through we use the notes as-is, and during the second time we
 * generate the notes again with the same properties except we alter the fifth
 * p-field up 4 semitones.
 *
 * Note: Altering a Notes values like this is alright for this example, but it
 * is a destructive edit. Real world code might make copies of Notes or alter
 * the score generation to maintain the original values.
 */
import csnd6.csnd6;
import csnd6.Csound;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Example6 {

    // Convert MIDI Note Numbers to Csound PCH format
    public static String midi2pch(int num) {
        return String.format("%d.%02d", 3 + (num / 12), num % 12);
    }

    public static class Note {

        List pfields;

        public Note(Object... args) {
            this.pfields = Arrays.asList(args);
        }

        public String toString() {
            StringBuilder buffer = new StringBuilder("i");
            for (int i = 0; i < pfields.size(); i++) {
                buffer.append(" ");
                if (i == 4) {
                    buffer.append(midi2pch(((Integer) pfields.get(i))));
                } else {
                    buffer.append(pfields.get(i).toString());
                }
            }
            return buffer.toString();
        }
    }

    public static String generateScore() {
        List<Note> vals = new ArrayList<Note>();

        // initialize a list to hold lists of values 
        for (int i = 0; i < 13; i++) {
            Note n = new Note(1, i * .25, .25, 0.5,
                    60 + (int) (Math.random() * 15));
            vals.add(n);
        }

        // convert list of values into a single string
        StringBuilder buffer = new StringBuilder();
        for (Note note : vals) {
            buffer.append(note.toString()).append("\n");
        }

        // generate notes again transposed a Major 3rd up
        for (Note note : vals) {
            int midiPch = (Integer) note.pfields.get(4);
            double start = (Double) note.pfields.get(1);
            note.pfields.set(4, midiPch + 4);
            note.pfields.set(1, start + 0.125);
            buffer.append(note.toString()).append("\n");
        }

        return buffer.toString();
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
                + "ipch = cps2pch(p5, 12)\n"
                + "kenv linsegr 0, .05, 1, .05, .7, .4, 0\n"
                + "aout vco2 p4 * kenv, ipch \n"
                + "aout moogladder aout, 2000, 0.25\n"
                + "outs aout, aout\n"
                + "endin\n";

        /* SCORE EXAMPLES */

        // Generate Score 
        String sco = generateScore();

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

        // The following is our main performance loop. We will perform one block 
        // of sound at a time and continue to do so while it returns 0, which 
        // signifies to keep processing.  We will explore this loop technique in 
        // further examples.

        while (c.PerformKsmps() == 0) {
            // pass for now
        }


        // stops Csound
        c.Stop();

        // clean up Csound; this is useful if you're going to reuse a Csound 
        // instance
        c.Cleanup();

    }
}
