; Example 3 - Using our own performance loop
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; In this example, we use a while loop to perform Csound one audio block at a time.
; This technique is important to know as it will allow us to do further processing
; safely at block boundaries.  We will explore the technique further in later examples.


(import (csnd6 csnd6 Csound))

; this line turns off Csound's atexit handler as well as signal handlers
(csnd6/csoundInitialize (bit-or csnd6/CSOUNDINIT_NO_ATEXIT csnd6/CSOUNDINIT_NO_SIGNAL_HANDLER))

; Defining our Csound ORC code within a multiline String
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

(let [c (Csound.)]
 (.SetOption c "-odac") ; Using SetOption() to configure Csound
                        ; Note: use only one commandline flag at a time
 (.CompileOrc c orc)    ; Compile the Csound Orchestra String
 (.ReadScore c sco)     ; Compile the Csound SCO String 
 (.Start c)             ; When compiling from strings, this call is necessary before doing any performing 
 
  ; The following is our main performance loop. We will perform one block of sound at a time 
  ; and continue to do so while it returns 0, which signifies to keep processing.  We will
  ; explore this loop technique in further examples. 
 (loop [retval 0]
   (when (zero? retval)
      (recur (.PerformKsmps c))))
 (.Stop c))

