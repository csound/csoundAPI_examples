;;;;
;;;;  Copyright (C) 2024 Victor Lazzarini
;;;;
;;;;  API Examples: control channels
;;;;  
;;;;  This file is part of Csound.
;;;;
;;;;  The Csound Library is free software; you can redistribute it
;;;;  and/or modify it under the terms of the GNU Lesser General Public
;;;;  License as published by the Free Software Foundation; either
;;;;  version 2.1 of the License, or (at your option) any later version.
;;;;
;;;;  Csound is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;  GNU Lesser General Public License for more details.
;;;;
;;;;  You should have received a copy of the GNU Lesser General Public
;;;;  License along with Csound; if not, write to the Free Software
;;;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;;;  02111-1307 USA
;;;;

;;; check for libcsound locations
;;; first on MacOS
(defvar *libcsound*
  (concatenate 'string (posix-getenv "HOME")
               "/Library/Frameworks/CsoundLib64.framework/CsoundLib64"))
(if (not (probe-file *libcsound*))
    (setf *libcsound*
          "/Library/Frameworks/CsoundLib64.framework/CsoundLib64"))
;;; then local directory - linux .so 
(if (not (probe-file *libcsound*))
    (setf *libcsound* "libcsound64.so"))
;;; if libcsound was not found
(if (not (probe-file *libcsound*)) (quit))

;;; sbcl FFI interface
(load-shared-object *libcsound*)
(define-alien-routine "csoundCreate" (* T) (a (* T)) (b c-string))
(define-alien-routine "csoundStart" int (a (* T)))
(define-alien-routine "csoundDestroy" void (a (* T)))
(define-alien-routine "csoundSetControlChannel" void (a (* T))  (b c-string) (c double))
(define-alien-routine "csoundCompileOrc" int (a (* T)) (b c-string) (c int))
(define-alien-routine "csoundEventString" int (a (* T)) (b c-string) (c int))
(define-alien-routine "csoundPerformKsmps" int (a (* T)))
(define-alien-routine "csoundSetOption" int (a (* T)) (b c-string))
(define-alien-routine "csoundGetKr" double (a (* T)))

(defvar *nl* (format nil "~c" #\linefeed))
(defvar *code* (concatenate 'string *nl*
                            "0dbfs = 1" *nl*
                            "instr 1" *nl*
                            "kp chnget \"pitch\" " *nl*
                            "a1 expon p4,p3,0.001" *nl*
                            "a2 oscil a1, cpsmidinn(p5)*kp" *nl*
                            "    out a2" *nl*
                            "endin" *nl* 
                            "icnt = 0" *nl*))
;;; run for 5 seconds
(defvar *dur* 5.)
;;; create the Csound engine instance
(defvar *cs* (csoundCreate NIL NIL))
;;; get command-line options
(loop for opt in (cdr *posix-argv*)
      do (csoundSetOption *cs* opt))
;;; compile the CSD
(if (= (csoundCompileOrc *cs* *code* 0) 0) 
    (if (= (csoundStart *cs*) 0)
        (let ((inc (/ 1. (* *dur* (csoundGetKr *cs*))))
              (pitch 1.0d0))  
          ;; send in events
          (csoundEventString *cs* (format nil "i1 0 ~f 0.1 60" *dur*) 0)
          (csoundEventString *cs* (format nil "e ~f" *dur*) 0)
          (csoundSetControlChannel *cs* "pitch" pitch)
          ;; compute audio blocks
          (loop while (= (csoundPerformKsmps *cs*) 0)
           do
           (csoundSetControlChannel *cs* "pitch" (incf pitch inc))))))
;;; destroy the engine instance
(csoundDestroy *cs*)

