;;;;
;;;;  Copyright (C) 2024 Victor Lazzarini
;;;;
;;;;  API Examples: multi-threaded performance
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
(define-alien-routine "csoundSleep" void (a int))
(define-alien-routine "csoundEventString" int (a (* T)) (b c-string) (c int))
(define-alien-routine "csoundCreatePerformanceThread" (* T) (a (* T)))
(define-alien-routine "csoundPerformanceThreadPlay" void (a (* T)))
(define-alien-routine "csoundPerformanceThreadIsRunning" int (a (* T)))
(define-alien-routine "csoundPerformanceThreadJoin" void (a (* T)))
(define-alien-routine "csoundDestroyPerformanceThread" void (a (* T)))

(defvar *nl* (format nil "~C" #\linefeed))
(defvar *code* (concatenate 'string *nl*
                            "0dbfs = 1" *nl*
                            "instr 1" *nl*
                            "icnt = 0 " *nl*
                            "while icnt < 12 do" *nl*
                            "schedule 2,icnt*p3,p3*2,p4,"
                            "cpsmidinn(icnt+p5)" *nl*
                            "icnt += 1" *nl*
                            "od" *nl*
                            "endin" *nl* 
                            "instr 2" *nl* 
                            "a1 expon p4,p3,0.001" *nl*
                            "a2 oscil a1,p5" *nl*
                            "    out a2" *nl*
                            "endin" *nl* 
                            ))

;;; create the Csound engine instance
(defvar *cs* (csoundCreate NIL NIL))
;;; enforce realtime audio and suppress messages
(csoundSetOption *cs* "-o dac -dm0")
;;; compile the CSD
(if (= (csoundCompileOrc *cs* *code* 0) 0)
    ;; create performance thread
    (let ((perf (csoundCreatePerformanceThread *cs*)))
      ;; start engine
      (if (= (csoundStart *cs*) 0)
          ;; start performance
          (csoundPerformanceThreadPlay perf))
      (let ((res (csoundPerformanceThreadIsRunning perf)) line)
        (loop
         while (= res 1)
         do
         ;; prompt for input
         (write-string "Csound>")
         (finish-output)
         ;; read and send event asynchronously
         ;; use event e <t> to finish after t secs
         (setf line (read-line))      
         (csoundEventString *cs* line 1)
         (if (string= (subseq line 0 1) "e") (setf res 0)))
        ;; join the performance thread
        (csoundPerformanceThreadJoin perf))
      ;; destroy the perfomance thread object
      (csoundDestroyPerformanceThread perf)))
;; destroy the engine
(csoundDestroy *cs*)




