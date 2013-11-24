package csoundapiexamples;

import csnd6.Csound;
import csnd6.CsoundPerformanceThread;
import csnd6.csnd6;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

/**
 * Example 11 - Graphical User Interfaces Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example demonstrates a minimal Graphical User Interface application. The
 * setup of Csound and starting of the CsoundPerformanceThread is done in the
 * global scripting space. Afterwards, a Swing GUI is created that has one
 * button. The button's actionListener sends a Csound SCO note to the
 * thread-safe InputMessage method of CsoundPerformanceThread.
 *
 * For this example, since there is no need to synchronize continuous channel
 * data changes with Csound, it is more efficient to use the
 * CsoundPerformanceThread, as it is a native thread.
 */
public class Example11 {

    public static void main(String[] args) {
        csnd6.csoundInitialize(
                csnd6.CSOUNDINIT_NO_ATEXIT | csnd6.CSOUNDINIT_NO_SIGNAL_HANDLER);

        // Defining our Csound ORC code within a String
        final String orc = "sr=44100\n"
                + "ksmps=32\n"
                + "nchnls=2\n"
                + "0dbfs=1\n"
                + "\n"
                + "instr 1 \n"
                + "kenv linsegr 0, .05, 1, .05, .9, .8, 0\n"
                + "aout vco2 p4 * kenv, p5\n"
                + "aout moogladder aout, 2000, p6\n"
                + "outs aout, aout\n"
                + "endin\n";

        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                JFrame frame = new JFrame("Csound API GUI Example");
                JButton button = new JButton("Play Note");

                final Csound c = new Csound();
                c.SetOption("-odac");
                c.SetOption("-m7");
                c.CompileOrc(orc);
                c.Start();

                final CsoundPerformanceThread pt = new CsoundPerformanceThread(c);
                pt.Play();

                button.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        pt.InputMessage("i1 0 2 .5 400 .25");
                    }
                });

                frame.addWindowListener(new WindowAdapter() {
                    @Override
                    public void windowClosing(WindowEvent e) {
                        pt.Stop();
                        pt.Join();
                        c.Stop();
                    }
                });

                frame.getContentPane().add(button);
                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                frame.pack();
                frame.setVisible(true);
            }
        });

    }
}
