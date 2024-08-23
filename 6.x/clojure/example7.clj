; Example 7 - Communicating continuous values with Csound's Channel System
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; This example introduces using Csound's Channel System to communicate 
; continuous control data (k-rate) from a host program to Csound. The 
; first thing to note is the random-line function. It takes in a base value
; and a range and returns a function that will vary randomly over time. 
; The function closes over a state map that uses atoms to hold mutable
; data. When the function finds dur has reached 0, it will call reset-line
; to calculate a new random target value (:end), a random duration in which to 
; run (:dur, expressed as # of audio blocks to last in duration), and
; calculates the increment value to apply to the current value per audio-block.
; 
; In this example, we use two random-lines's, one for amplitude and 
; another for frequency.  We start a Csound instrument instance that reads
; from two channels using the chnget opcode. In turn, we update the values
; to the channel from the host program.  In this case, because we want to 
; keep our values generating in sync with the audio engine, we use a 
; loop instead of a CsoundPerformanceThread. To update the channel,
; we call the SetChannel method on the Csound object, passing a channel name
; and value.  Note: The function that random-line returns not only gets
; us the current value, but also advances the internal state by the increment
; and also decrements the :dur counter.

(import [csnd6 csnd6 Csound] 
        [java.util Random])

; this line turns off Csound's atexit handler as well as signal handlers
(csnd6/csoundInitialize (bit-or csnd6/CSOUNDINIT_NO_ATEXIT csnd6/CSOUNDINIT_NO_SIGNAL_HANDLER))

(defn reset-line [state]
  "Calculates new target value, duration and increment"
  (let [r (Random.)]
    (reset! (:dur state) (+ 256 (.nextInt r 256)))
    (reset! (:end state) (.nextDouble r))
    (reset! (:increment state) 
            (/ (- @(:end state) @(:curval state)) @(:dur state)))
    state))

(defn random-line [base range] 
  "Returns a function that will vary in time with the given base
  value and range"
  (let [state  (reset-line 
                 { :curval (atom 0.0)
                  :increment (atom 0)
                  :end (atom 0)
                  :dur (atom 0)})]
    (fn [] 
      (swap! (:dur state) dec)
      (when (<= @(:dur state) 0)
          (reset-line state))
      (let [c @(:curval state)]
        (swap! (:curval state) #(+ % @(:increment state))) 
        (+ base (* range c))))))

; Defining our Csound ORC code within a multiline String
(def orc "
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
kamp chnget \"amp\"
kfreq chnget \"freq\"
printk 0.5, kamp
printk 0.5, kfreq
aout vco2 kamp, kfreq
aout moogladder aout, 2000, 0.25
outs aout, aout
endin")


(def sco "i1 0 60")

(let [c (Csound.)
      amp (random-line 0.4 0.2)
      freq (random-line 400.0 80.0)]
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
      (.SetChannel c "amp" (amp))
      (.SetChannel c "freq" (freq))
      (recur (.PerformKsmps c))))
  (.Stop c))


