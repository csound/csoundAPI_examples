; Example 11 - Graphical User Interfaces
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; This example demonstrates a minimal Graphical User Interface application.
; The setup of Csound and starting of the CsoundPerformanceThread is done in 
; the global scripting space.  Afterwards, a Swing GUI is created that has
; one button.  The button's actionListener sends a Csound SCO note to the 
; thread-safe InputMessage method of CsoundPerformanceThread. 
;
; For this example, since there is no need to synchronize continous channel data
; changes with Csound, it is more efficient to use the CsoundPerformanceThread,
; as it is a native thread.  

(import [javax.swing JFrame JButton])
(import [java.awt.event ActionListener WindowAdapter])
(import [csnd6 csnd6 Csound CsoundPerformanceThread] 
        [java.util Random])


; this line turns off Csound's atexit handler as well as signal handlers
(csnd6/csoundInitialize 
  (bit-or csnd6/CSOUNDINIT_NO_ATEXIT 
          csnd6/CSOUNDINIT_NO_SIGNAL_HANDLER))

; Our Orchestra for our project

(def orc "
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
kenv linsegr 0, .05, 1, .05, .9, .8, 0
aout vco2 p4 * kenv, p5
aout moogladder aout, 2000, p6
outs aout, aout
endin")

(def c (Csound.)) ; create an instance of Csound

(doto c
  (.SetOption "-odac")  ; Set option for Csound 
  (.SetOption "-m7")    ; Set option for Csound 
  (.CompileOrc orc)     ; Compile Orchestra from String
  (.Start)              ; When compiling from strings, this call is necessary before doing any performing 
  )

(def pt (CsoundPerformanceThread. c))
(.Play pt)

(let [frame (JFrame. "Csound API GUI Example")
      button (JButton. "Play Note")]
  (.addActionListener button 
                      (proxy [ActionListener] []
                        (actionPerformed [evt]
                          (.InputMessage pt "i1 0 2 .5 400 .25"))))
  (.addWindowListener frame
                      (proxy [WindowAdapter] []
                        (windowClosing [evt]
                          (.Stop pt)
                          (.Join pt))))
  (.. frame getContentPane (add button))
  (doto frame
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.setVisible true)))

