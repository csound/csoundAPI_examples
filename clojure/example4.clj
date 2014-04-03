; Example 4 - Using Csound's Performance Thread 
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; In this example, we use a CsoundPerformanceThread to run Csound in 
; a native thread.  Using a native thread is important to get the best
; runtime performance for the audio engine. CsoundPerformanceThread has
; some convenient methods for handling events, but does not have
; features for doing regular processing at block boundaries.  In general,
; use CsoundPerformanceThread when the only kinds of communication you
; are doing with Csound are through events, and not using channels.


(import (csnd6 csnd6 Csound CsoundPerformanceThread))

; this line turns off Csound's atexit handler as well as signal handlers
(csnd6/csoundInitialize (bit-or csnd6/CSOUNDINIT_NO_ATEXIT csnd6/CSOUNDINIT_NO_SIGNAL_HANDLER))

; Defining our Csound ORC code within a String
(def orc "
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
aout vco2 0.5, 440
outs aout, aout
endin")


; Defining our Csound SCO code 
(def sco "i1 0 1")

; Create a new Csound Object
; Create a new CsoundPerformanceThread, passing in the Csound object  
(let [c (Csound.)
      pt (CsoundPerformanceThread. c)]
  (.SetOption c "-odac") ; Using SetOption() to configure Csound
                         ; Note: use only one commandline flag at a time
  (.CompileOrc c orc)    ; Compile the Csound Orchestra String
  (.ReadScore c sco)     ; Compile the Csound SCO String 
  (.Start c)             ; When compiling from strings, this call is necessary before doing any performing 

; starts the thread, which is now running separately from the main thread. This 
; call is asynchronous and will immediately return back here to continue code
; execution.
  (.Play pt)

; Join will wait for the other thread to complete. If we did not call Join(),
; after t.Play() returns we would immediate move to the next line, c.Stop(). 
; That would stop Csound without really giving it time to run. 
  (.Join pt)
  (.Stop c)
  (.Cleanup c))



