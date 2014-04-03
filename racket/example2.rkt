#lang racket

; Example 2 - Compilation with Csound without CSD
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; In this example, we move from using an external CSD file to 
; embedding our Csound ORC and SCO code within our Racket project.
; Besides allowing encapsulating the code within the same file,
; using the CompileOrc() and CompileSco() API calls is useful when
; the SCO or ORC are generated, or perhaps coming from another 
; source, such as from a database or network.

(require "cs6ffi.rkt")

; this line turns off Csound's atexit handler as well as signal handlers
(csound-initialize 3)

; Defining our Csound ORC code within a here-doc 
(define orc #<<EOF
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
aout vco2 0.5, 440
outs aout, aout
endin
EOF
)


; Defining our Csound SCO code 
(define sco "i1 0 1")


(let ([cs (csound-create)] ; Create an instance of the Csound object 

      [args '("csound" "test1.csd")]) ; Create args as list

  (csound-set-option cs "-odac") ; Using SetOption() to configure Csound 
                                 ; Note: use only one commandline flag at a time
  (csound-compile-orc cs orc)     ; Compile the Csound Orchestra String
  (csound-read-score cs sco)      ; Compile the Csound SCO String 
  (csound-start cs)               ; When compiling from strings, this call is necessary before doing any performing 
  (csound-perform cs)            ; This call runs Csound to completion
  (csound-stop cs))              ; At this point, Csound is already stopped, but this call is here
                                 ; as it is something that you would generally call in real-world 
                                 ; contexts 


