;;;;
;;;;  Copyright (C) 2024 Victor Lazzarini
;;;;
;;;;  API Examples: compiling from a string
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
(define-alien-routine "csoundCompileOrc" int (a (* T)) (b c-string) (c int))
(define-alien-routine "csoundSetOption" int (a (* T)) (b c-string))
(define-alien-routine "csoundStart" int (a (* T)))
(define-alien-routine "csoundPerformKsmps" int (a (* T)))
(define-alien-routine "csoundDestroy" void (a (* T)))

(defvar *nl* (format nil "~C" #\linefeed))
(defvar *code* (concatenate 'string *nl*
  "0dbfs = 1" *nl*
  "instr 1" *nl* 
  "a1 expon p4,p3,0.001" *nl*
  "a2 oscil a1, p5" *nl*
  "    out a2" *nl*
  "endin" *nl* 
  "icnt = 0" *nl*
  "while icnt <= 12 do" *nl* 
  " schedule 1, icnt*0.25, 0.3, 0.1, cpsmidinn(60+icnt)" *nl*
  " icnt += 1" *nl*
  "od" *nl*
  "event_i \"e\", icnt*0.3" *nl*))

;;; create the Csound engine instance
(defvar *cs* (csoundCreate NIL NIL))
;;; get command-line options
(loop for opt in (cdr *posix-argv*)
   do (csoundSetOption *cs* opt))
;;; compile the CSD
(if (= (csoundCompileOrc *cs* *code* 0) 0)
    ;; start engine
    (if (= (csoundStart *cs*) 0)
          ;; compute audio
          (loop while (= (csoundPerformKsmps *cs*) 0))))
;;; destroy the engine instance
(csoundDestroy *cs*)


