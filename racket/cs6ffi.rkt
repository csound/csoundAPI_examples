#lang racket

; Csound 6 FFI Interface
; Author: Steven Yi<stevenyi@gmail.com>

(provide csound-create csound-compile csound-perform csound-stop)
        
(require racket/system
         ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-csound (ffi-lib "libcsound64"))

(define _CSOUND-pointer (_cpointer 'CSOUND))


(define-csound csoundCreate (_fun -> _CSOUND-pointer))
(define-csound csoundCompile (_fun _CSOUND-pointer _int  (_array/list _string 2) -> _int))
(define-csound csoundPerform (_fun _CSOUND-pointer -> _int))
(define-csound csoundStop (_fun _CSOUND-pointer -> _void))

(define csound-create csoundCreate)

(define (csound-compile cs args)
  (csoundCompile cs (length args) args))


(define csound-perform csoundPerform)
(define csound-stop csoundStop)
