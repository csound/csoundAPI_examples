#lang racket

; Example 3 - Using our own performance loop
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; In this example, we use a recursive function to perform
; Csound one audio block at a time.  This technique is
; important to know as it will allow us to do further
; processing safely at block boundaries.  We will explore the
; technique further in later examples.


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

; Define our custom performance function
; uses recursion to continuously call csound-perform-ksmps 
(define (my-perform csnd)
  (let ([retVal (csound-perform-ksmps csnd)]) 
    (if (= 0 retVal)
      (my-perform csnd)
      retVal)))

(let ([cs (csound-create)] ; Create an instance of the Csound object 

      [args '("csound" "test1.csd")]) ; Create args as list

  (csound-set-option cs "-odac") ; Using SetOption() to configure Csound 
                                 ; Note: use only one commandline flag at a time
  (csound-compile-orc cs orc)     ; Compile the Csound Orchestra String
  (csound-read-score cs sco)      ; Compile the Csound SCO String 
  (csound-start cs)               ; When compiling from strings, this call is necessary before doing any performing 

  (my-perform cs)            ; Perform Csound one ksmps at a time using custom performance function

  (csound-stop cs))              ; At this point, Csound is already stopped, but this call is here
                                 ; as it is something that you would generally call in real-world 
                                 ; contexts 



