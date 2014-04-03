#lang racket

; Example 1 - Simple Compilation with Csound
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.30
;
; This example is a barebones example for creating an instance of Csound, 
; compiling a pre-existing CSD, calling Perform to run Csound to completion,
; then Stop and exit.  
;
; The first thing we do is import the cs6ffi file, which is the module 
; containing the Racket interface to the Csound API.

(require "cs6ffi.rkt")

; this line turns off Csound's atexit handler as well as signal handlers
(csound-initialize 3)

(let ([cs (csound-create)] ; Create an instance of the Csound object 

      [args '("csound" "test1.csd")]) ; Create args as list
  
  (csound-compile cs args) ; Compile a pre-defined test1.csd file

  (csound-perform cs)      ; This call runs Csound to completion

  (csound-stop cs))        ; At this point, Csound is already stopped, but this call is here
                           ; as it is something that you would generally call in real-world 
                           ; contexts 
