; Example 6 - Generating Score
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; This example continues on from Example 5, rewriting the example using
; a small function called note that returns a vector of values. 
; A note->str function is provided the formats the note's values 
; into a well-formatted Csound SCO note.  
;
; This example also shows how a list of notes can be transformed. 
; We first generate notes using mapping over a range from 0 to 12.  Next,
; we map over the notes to generate a transposed set of notes. These
; notes have the same properties as the original notes except the 
; fifth p-field has been transposed up 4 semitones. 

(import [csnd6 csnd6 Csound] 
        [java.util Random])
(require '[clojure.string :refer [join]])

; this line turns off Csound's atexit handler as well as signal handlers
(csnd6/csoundInitialize (bit-or csnd6/CSOUNDINIT_NO_ATEXIT csnd6/CSOUNDINIT_NO_SIGNAL_HANDLER))

(defn midi->pch [num]
  "Convert MIDI Note Numbers to Csound PCH format"
  (format "%d.%02d" (+ 3 (quot num 12)) (rem num 12)))

(defn note [& pfields]
  "Takes arguments and puts into a vector to represent a note"
  (into [] pfields))

(defn note->str [note]
  "Converts a note into a string"
  (let [pfields (pop note)
        mpch (last note)]
   (str "i" (join " " pfields) " " (midi->pch mpch))))

; Defining our Csound ORC code within a multiline String
(def orc "
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
ipch = cps2pch(p5, 12)
kenv linsegr 0, .05, 1, .05, .7, .4, 0
aout vco2 p4 * kenv, ipch 
aout moogladder aout, 2000, 0.25
outs aout, aout
endin")


; generate notes using random MIDI pitch values from 60-75
(def notes
  (let [r (Random.)]
    (map #(note 1 (* 0.25 %) 0.25 0.5 
                (+ 60 (.nextInt r 15))) 
         (range 0 13))))

; generate a tansposed set of notes  
(def transposed
  (map #(let [fields (pop %) 
              mpch (last %)] 
          (conj fields (+ 4 mpch))) notes))

; convert both sets of notes into a single SCO string
(def sco 
  (join "\n"
    (map note->str 
      (into notes transposed))))

(let [c (Csound.)]
  (.SetOption c "-odac") ; Using SetOption() to configure Csound
  ; Note: use only one commandline flag at a time
  (.CompileOrc c orc)    ; Compile the Csound Orchestra String
  (.ReadScore c sco)     ; Read in Score from pre-written String 
  (.Start c)             ; When compiling from strings, this call is necessary before doing any performing 

  ; The following is our main performance loop. We will perform one block of sound at a time 
  ; and continue to do so while it returns 0, which signifies to keep processing.  We will
  ; explore this loop technique in further examples. 
  (loop [retval 0]
    (when (zero? retval)
      (recur (.PerformKsmps c))))
  (.Stop c))

