; Example 1 - Simple Compilation with Csound
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.30
;
; This example is a barebones example for creating an instance of Csound, 
; compiling a pre-existing CSD, calling Perform to run Csound to completion,
; then Stop and exit.  
;
; The first thing we do is import the csnd6 module, which is the module 
; containing the Java interface to the Csound API.

(import (csnd6 csnd6 Csound))

; this line turns off Csound's atexit handler as well as signal handlers
(csnd6/csoundInitialize (bit-or csnd6/CSOUNDINIT_NO_ATEXIT csnd6/CSOUNDINIT_NO_SIGNAL_HANDLER))

(let [c (Csound.)]      ; Create an instance of the Csound object 
  (.Compile c "test1.csd")    ; Compile a pre-defined test1.csd file
  (.Perform c)                ; This call runs Csound to completion
  (.Stop c))                  ; At this point, Csound is already stopped, but this call is here
                              ; as it is something that you would generally call in real-world 
                              ; contexts 
