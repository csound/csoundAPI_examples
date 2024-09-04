;;;;
;;;;  Copyright (C) 2024 Victor Lazzarini
;;;;
;;;;  API Examples: simple CLI frontend
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
(define-alien-routine "csoundCompile" int (a (* T)) (b int) (c (* c-string)))
(define-alien-routine "csoundStart" int (a (* T)))
(define-alien-routine "csoundPerformKsmps" int (a (* T)))
(define-alien-routine "csoundDestroy" void (a (* T)))

;;; command-line args (max 32)
(defvar *argc* (length *posix-argv*))
(let ((args (make-alien (array c-string 32))))
(loop for n from 0 to *argc*
      do (setf (deref (deref args) n)
               (nth n *posix-argv*)))
(defvar *argv* (cast args (* c-string))))

;;; create the Csound engine instance
(defvar *cs* (csoundCreate NIL NIL))
;;; compile the CSD
(if (= (csoundCompile *cs* *argc* *argv*) 0)
    ;; start engine
    (if (= (csoundStart *cs*) 0)
            (loop while (= (csoundPerformKsmps *cs*) 0))))
;;; destroy the engine instance
(csoundDestroy *cs*)
(free-alien *argv*)


