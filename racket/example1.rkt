#lang racket

(require "cs6ffi.rkt")

(define cs (csound-create))
(define args '("csound" "test1.csd"))
(csound-compile cs args)
(csound-perform cs)
(csound-stop cs)

