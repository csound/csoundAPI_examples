#!/usr/local/bin/sbcl --dynamic-space-size 500 --script
;;;;
;;;;  Copyright (C) 2024 Victor Lazzarini
;;;;
;;;;  API Examples:compiling code on-the-fly
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
  a1 expon p4,p3,0.001
  a2 oscil a1, p5
      out a2
  endin
  icnt = 0
  while icnt <= 12 do
   schedule 1, icnt*0.25, 0.3, 0.1, cpsmidinn(60+icnt)
   icnt += 1
   od
  event_i \"e\", icnt*0.3")

(defvar *perf*
  "icnt = 0
    while icnt < 12 do
    schedule 1,icnt*0.1,0.3,0.1,
    cpsmidinn(icnt+60)
    icnt += 1
   od")

;;; create the Csound engine instance
(defvar *cs* (csound-create))
;;; get command-line options
(loop for opt in (cdr *posix-argv*)
   do (csound-set-option *cs* opt))
;;; compile the CSD
(if (= (csound-compile-orc *cs* *code*) 0)
    ;; start engine
    (if (= (csound-start *cs*) 0)
        (let ((time 0.0) (inc (/ 1.0 (csound-get-kr *cs*))))
          (loop while (= (csound-perform-ksmps *cs*) 0)
                ;; run audio computing
                do
                (incf time inc)
                (if (> time 2.5)
                    (progn
                      (csound-compile-orc *cs* *perf*)
                      (incf time -2.5)))))))
;;; destroy the engine instance
(csound-destroy *cs*)


