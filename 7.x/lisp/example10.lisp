#!/usr/local/bin/sbcl --dynamic-space-size 500 --script
;;;;
;;;;  Copyright (C) 2024 Victor Lazzarini
;;;;
;;;;  API Examples: compiling from CSD string and resetting
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
(defvar *nl* (format nil "~C" #\linefeed))
(defvar *code*
  (concatenate 'string *nl*
               "<CsoundSynthesizer>" *nl*
               "<CsOptions>" *nl*
               "-odac" *nl*
               "</CsOptions>" *nl*
               "<CsInstruments>" *nl*
               "0dbfs = 1  " *nl* 
               "instr 1    " *nl*
               "a1 expon p4,p3,0.001" *nl*
               "a2 oscil a1, p5" *nl*
               "    out a2" *nl*
               "endin" *nl* 
               "icnt = 0" *nl*
               "while icnt <= 12 do" *nl*
               " schedule 1, icnt*0.25, 0.3, 0.1,"
               "cpsmidinn(60+icnt)" *nl*
               " icnt += 1 " *nl*
               "od" *nl*
               "event_i \"e\", icnt*0.3" *nl*
               "</CsInstruments>" *nl*
               "</CsoundSynthesizer>" *nl*))

;;; create the Csound engine instance
(defvar *cs* (csound-create))
;;; get command-line options
(loop for opt in (cdr *posix-argv*)
   do (csound-set-option *cs* opt))
;;; compile the CSD
(dotimes (n 2)
  (if (= (csound-compile-csd *cs* *code* :is-text 1) 0)
      ;; start engine
      (if (= (csound-start *cs*) 0)
          ;; compute audio
          (loop while (= (csound-perform-ksmps *cs*) 0))))
  (csound-reset *cs*)
  )
;;; destroy the engine instance
(csound-destroy *cs*)


