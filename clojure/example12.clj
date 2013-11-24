; Example 12 - Graphical User Interfaces
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; This example demonstrates a slightly more advanced GUI example. 
; It uses a slider to allow setting the value of the frequency that
; the notes initiated by the button will play at.  
;
; Note: the actual use of update() here is not thread-safe.  In 
; real-world usage, we would need to drive Csound from a loop calling
; PerformKsmps to ensure thread-safety.  For this example, the updating
; generally works as there are few things demanding computation. 
; Also to note, using SetChannel on the Csound Object would be thread
; safe, as Csound internally protects channels with a spin lock. 


(import [javax.swing JFrame JButton JSlider])
(import [javax.swing.event ChangeListener])
(import [java.awt BorderLayout])
(import [java.awt.event ActionListener WindowAdapter])
(import [csnd6 csnd6 Csound CsoundPerformanceThread 
         CsoundMYFLTArray controlChannelType] 
        [java.util Random])


; this line turns off Csound's atexit handler as well as signal handlers
(csnd6/csoundInitialize 
  (bit-or csnd6/CSOUNDINIT_NO_ATEXIT 
          csnd6/CSOUNDINIT_NO_SIGNAL_HANDLER))


(def ctrl-chan-type 
  (bit-or (.swigValue controlChannelType/CSOUND_INPUT_CHANNEL) 
          (.swigValue controlChannelType/CSOUND_CONTROL_CHANNEL)))


(defn create-channel [csound channel-name]
  "Creates a Csound channel and returns a CsoundMYFLTArray wrapper object"
  (let [chn (CsoundMYFLTArray. 1)]
    (.GetChannelPtr csound (.GetPtr chn) channel-name ctrl-chan-type)
    chn))

; Our Orchestra for our project

(def orc "
sr=44100
ksmps=32
nchnls=2
0dbfs=1

gkpch chnexport \"freq\", 1

instr 1 
kpch port gkpch, 0.01, i(gkpch)
printk .05, gkpch
kenv linsegr 0, .05, 1, .05, .9, .8, 0
aout vco2 p4 * kenv, kpch
aout moogladder aout, 2000, .25 
outs aout, aout
endin")

(def c (Csound.)) ; create an instance of Csound

(doto c
  (.SetOption "-odac")  ; Set option for Csound 
  (.SetOption "-m7")    ; Set option for Csound 
  (.CompileOrc orc)     ; Compile Orchestra from String
  (.Start)              ; When compiling from strings, this call is necessary before doing any performing 
  )

(def freq-channel (create-channel c "freq"))

(def pt (CsoundPerformanceThread. c))
(.Play pt)

(let [frame (JFrame. "Csound API GUI Example")
      button (JButton. "Play Note")
      slider (JSlider. 80 600)
      ]
  (.SetValue freq-channel 0 (.getValue slider))
  (.addActionListener button 
                      (proxy [ActionListener] []
                        (actionPerformed [evt]
                          (.InputMessage pt "i1 0 2 .5"))))
  (.addWindowListener frame
                      (proxy [WindowAdapter] []
                        (windowClosing [evt]
                          (.Stop pt)
                          (.Join pt))))
  (.addChangeListener slider
                     (proxy [ChangeListener] []
                       (stateChanged [evt]
                         (.SetValue freq-channel 0 (.getValue slider)))))

  (.. frame getContentPane (add button BorderLayout/NORTH))
  (.. frame getContentPane (add slider))
  (.SetValue freq-channel 0 (.getValue slider)) 
  (doto frame
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.pack)
    (.setVisible true)))


