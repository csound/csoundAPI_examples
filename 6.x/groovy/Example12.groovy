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

import csnd6.Csound;
import csnd6.CsoundMYFLTArray;
import csnd6.CsoundPerformanceThread;
import csnd6.controlChannelType;
import csnd6.csnd6;
import groovy.swing.SwingBuilder
import java.awt.*
import javax.swing.*
import javax.swing.event.*


/**
  * Creates a Csound Channel and returns a CsoundMYFLTArray wrapper object
  */
def createChannel(Csound csound, String channelName) {
    def channel = new CsoundMYFLTArray(1);

    csound.GetChannelPtr(channel.GetPtr(), channelName,
            controlChannelType.CSOUND_CONTROL_CHANNEL.swigValue()
            | controlChannelType.CSOUND_INPUT_CHANNEL.swigValue());

    return channel;
}

csnd6.csoundInitialize(
        csnd6.CSOUNDINIT_NO_ATEXIT | csnd6.CSOUNDINIT_NO_SIGNAL_HANDLER);

// Defining our Csound ORC code within a String
final String orc = """
    sr=44100
    ksmps=32
    nchnls=2
    0dbfs=1

    gkpch chnexport "freq", 1

    instr 1
    kpch port gkpch, 0.05, i(gkpch)
    printk .05, gkpch
    kenv linsegr 0, .05, 1, .01, .9, .8, 0
    aout vco2 p4 * kenv, kpch
    aout moogladder aout, 2000, .25
    outs aout, aout
    endin"""


new SwingBuilder().edt {

    def c = new Csound()
    c.SetOption("-odac")
    c.SetOption("-m7")
    c.CompileOrc(orc)
    c.Start()

    def pt = new CsoundPerformanceThread(c)
    pt.Play()

    def freqChannel = createChannel(c, "freq");


    f = frame(title: 'Csound API GUI Example', 
          defaultCloseOperation:JFrame.EXIT_ON_CLOSE, 
          pack:true,
          show: true,
          windowClosing: {
              pt.Stop()
              pt.Join()
              c.Stop()
          }) {
      borderLayout()
      button(text: 'Play Note',  
          actionPerformed: { 
              pt.InputMessage("i1 0 2 .5 400 .25")
          }, constraints: BorderLayout.NORTH)
                //freqChannel.SetValue(0, slider.getValue());
      s = slider(minimum: 80, maximum: 600)
    }

    freqChannel.SetValue(0, s.value)
    s.addChangeListener(new ChangeListener() { 
        void stateChanged(ChangeEvent ce) {
            freqChannel.SetValue(0, ce.source.value)
        }
    })
}

