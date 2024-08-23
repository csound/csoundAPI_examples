package csoundapiexamples;

import csnd6.Csound;
import csnd6.CsoundMYFLTArray;
import csnd6.CsoundPerformanceThread;
import csnd6.controlChannelType;
import csnd6.csnd6;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JSlider;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * Example 12 - Graphical User Interfaces Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example demonstrates a slightly more advanced GUI example. It uses a
 * slider to allow setting the value of the frequency that the notes initiated
 * by the button will play at.
 *
 * Note: the actual use of SetValue() here is not thread-safe. In real-world
 * usage, we would need to drive Csound from a loop calling PerformKsmps to
 * ensure thread-safety. For this example, the updating generally works as there
 * are few things demanding computation. Also to note, using SetChannel on the
 * Csound Object would be thread safe, as Csound internally protects channels
 * with a spin lock.
 */
public class Example12 {

    /**
     * Creates a Csound Channel and returns a CsoundMYFLTArray wrapper object
     */
    public static CsoundMYFLTArray createChannel(Csound csound, String channelName) {
        CsoundMYFLTArray channel = new CsoundMYFLTArray(1);

        csound.GetChannelPtr(channel.GetPtr(), channelName,
                controlChannelType.CSOUND_CONTROL_CHANNEL.swigValue()
                | controlChannelType.CSOUND_INPUT_CHANNEL.swigValue());

        return channel;
    }

    public static void main(String[] args) {
        csnd6.csoundInitialize(
                csnd6.CSOUNDINIT_NO_ATEXIT | csnd6.CSOUNDINIT_NO_SIGNAL_HANDLER);

        // Defining our Csound ORC code within a String
        final String orc = "sr=44100\n"
                + "ksmps=32\n"
                + "nchnls=2\n"
                + "0dbfs=1\n"
                + "\n"
                + "gkpch chnexport \"freq\", 1\n"
                + "\n"
                + "instr 1 \n"
                + "kpch port gkpch, 0.05, i(gkpch)\n"
                + "printk .05, gkpch\n"
                + "kenv linsegr 0, .05, 1, .01, .9, .8, 0\n"
                + "aout vco2 p4 * kenv, kpch\n"
                + "aout moogladder aout, 2000, .25 \n"
                + "outs aout, aout\n"
                + "endin";

        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                JFrame frame = new JFrame("Csound API GUI Example");
                JButton button = new JButton("Play Note");
                final JSlider slider = new JSlider(80, 600);
                
                final Csound c = new Csound();
                c.SetOption("-odac");
                c.SetOption("-m7");
                c.CompileOrc(orc);
                c.Start();

                final CsoundMYFLTArray freqChannel = createChannel(c, "freq");
                freqChannel.SetValue(0, slider.getValue());
                
                final CsoundPerformanceThread pt = new CsoundPerformanceThread(c);
                pt.Play();

                button.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        pt.InputMessage("i1 0 2 .5");
                    }
                });

                slider.addChangeListener(new ChangeListener() {

                    @Override
                    public void stateChanged(ChangeEvent e) {
                        freqChannel.SetValue(0, slider.getValue());
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

                frame.getContentPane().add(button, BorderLayout.NORTH);
                frame.getContentPane().add(slider);
                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                frame.pack();
                frame.setVisible(true);
            }
        });

    }
}
