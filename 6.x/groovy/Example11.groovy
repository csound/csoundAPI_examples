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

import csnd6.Csound
import csnd6.CsoundPerformanceThread
import csnd6.csnd6
import groovy.swing.SwingBuilder
import java.awt.*
import javax.swing.*

csnd6.csoundInitialize(
    csnd6.CSOUNDINIT_NO_ATEXIT | csnd6.CSOUNDINIT_NO_SIGNAL_HANDLER)

// Defining our Csound ORC code within a String
def orc = """
    sr=44100
    ksmps=32
    nchnls=2
    0dbfs=1

    instr 1
    kenv linsegr 0, .05, 1, .05, .9, .8, 0
    aout vco2 p4 * kenv, p5
    aout moogladder aout, 2000, p6
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

    frame(title: 'Csound API GUI Example', 
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
          }, constraints: BorderLayout.CENTER)
      
    }
}

