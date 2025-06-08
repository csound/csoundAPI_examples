#!/usr/local/bin/sbcl --dynamic-space-size 500 --script
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

(load "csound-sbcl.lisp")
(use-package 'csound)

(defvar *code*
   "0dbfs = 1
    instr 1
     kp chnget \"pitch\"
     a1 expon p4,p3,0.001
     a2 oscil a1, cpsmidinn(p5)*kp
        out a2
    endin")
;;; run for 5 seconds
(defvar *dur* 5.)
;;; create the Csound engine instance
(defvar *cs* (csound-create))
;;; get command-line options
(loop for opt in (cdr *posix-argv*)
   do (csound-set-option *cs* opt))
;;; compile the CSD
(if (= (csound-compile-orc *cs* *code*) 0)
    ;; start engine
    (if (= (csound-start *cs*) 0)
        (let ((inc (/ 1. (* *dur* (csound-get-kr *cs*))))
              (pitch 1.0d0))  
          ;; send in events
          (csound-event-string *cs* (format nil "i1 0 ~f 0.1 60" *dur*))
          (csound-event-string *cs* (format nil "e ~f" *dur*))
          (csound-set-control-channel *cs* "pitch" pitch)
          ;; compute audio blocks
          (loop while (= (csound-perform-ksmps *cs*) 0)
           do
           (csound-set-control-channel *cs* "pitch" (incf pitch inc))))))
(format t "~Cpitch: ~3,1f ~C" #\Linefeed (csound-get-control-channel *cs* "pitch") #\Linefeed)
;;; destroy the engine instance
(csound-destroy *cs*)

