; TODO - fix comments
;
; Example 6 - Generating Score
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; This example continues on from Example 5, rewriting the example using
; a Class called Note. The note example has its __str__ method implemented
; to generate a well-formatted Csound SCO note.  
;
; This example also shows how a list of notes could be used multiple times.
; The first loop through we use the notes as-is, and during the second time
; we generate the notes again with the same properties except we alter the 
; fifth p-field up 4 semitones. 
;
; Note: Altering a Notes values like this is alright for this example, but 
; it is a destructive edit.  Real world code might make copies of Notes or 
; alter the score generation to maintain the original values. 

(import [csnd6 csnd6 Csound] 
        [java.util Random])
(require '[clojure.string :refer [join]])

; this line turns off Csound's atexit handler as well as signal handlers
(csnd6/csoundInitialize (bit-or csnd6/CSOUNDINIT_NO_ATEXIT csnd6/CSOUNDINIT_NO_SIGNAL_HANDLER))

(defn midi->pch [num]
  "Convert MIDI Note Numbers to Csound PCH format"
  (format "%d.%02d" (+ 3 (quot num 12)) (rem num 12)))

(defn note [& pfields]
  "takes arguments and puts into a vector to represent a note"
  (into [] pfields))

(defn note->str [note]
  (let [pfields (pop note)
        mpch (last note)]
   (str "i" (join " " pfields) " " (midi->pch mpch))))

; Defining our Csound ORC code within a triple-quoted, multline String
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


(def notes
  (let [r (Random.)]
    (map #(note 1 (* 0.25 %) 0.25 0.5 
                (+ 60 (.nextInt r 15))) 
         (range 0 13))))

(def transposed
  (map #(let [fields (pop %) 
              mpch (last %)] 
          (conj fields (+ 4 mpch))) notes))

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

;notes = []           #initialize a list to hold lists of values 
;for i in range(13): #populate that list
;    notes.append( Note(1, i * .25, .25, 0.5, randint(60,75)) )

;# now convert list of Note objects to string
;sco = ""
;for n in notes:
;    sco += "%s\n"%n # this implicitly calls the __str__ method on the Note Class

;# generate notes again transposed a Major 3rd up
;for n in notes:
;    n.pfields[4] += 4
;    n.pfields[1] += .125
;    sco += "%s\n"%n 





