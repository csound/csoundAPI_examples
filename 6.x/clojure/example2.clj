; Example 2 - Compilation with Csound without CSD
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; In this example, we move from using an external CSD file to 
; embedding our Csound ORC and SCO code within our Clojure project.
; Besides allowing encapsulating the code within the same file,
; using the CompileOrc() and CompileSco() API calls is useful when
; the SCO or ORC are generated, or perhaps coming from another 
; source, such as from a database or network.

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
 (.Perform c)           ; Run Csound to completion
 (.Stop c))

