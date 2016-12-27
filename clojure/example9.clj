; Example 9 - More efficient Channel Communications
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; This example continues on from Example 9 and just refactors the
; creation and setup of CsoundMYFLTArray's into a create-channel() 
; function.  This example illustrates some natural progression that
; might occur in your own API-based projects, and how you might 
; simplify your own code.


(import [csnd6 csnd6 Csound CsoundMYFLTArray controlChannelType] 
        [java.util Random])

; this line turns off Csound's atexit handler as well as signal handlers
(csnd6/csoundInitialize 
  (bit-or csnd6/CSOUNDINIT_NO_ATEXIT 
          csnd6/CSOUNDINIT_NO_SIGNAL_HANDLER))

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


(def ctrl-chan-type 
  (bit-or (.swigValue controlChannelType/CSOUND_INPUT_CHANNEL) 
          (.swigValue controlChannelType/CSOUND_CONTROL_CHANNEL)))


(defn create-channel [csound channel-name]
  "Creates a Csound channel and returns a CsoundMYFLTArray wrapper object"
  (let [chn (CsoundMYFLTArray. 1)]
    (.GetChannelPtr csound (.GetPtr chn) channel-name ctrl-chan-type)
    chn))

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

; define Csound input Control Channel type
; uses .swigValue to get the int values 
(let [c (Csound.)
      amp (random-line 0.4 0.2)
      freq (random-line 400.0 80.0)
      amp-channel (create-channel c "amp")   ; Uses create-channel to create a channel  
      freq-channel (create-channel c "freq")] ; and get a CsoundMFLTArray 

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
      (.SetValue amp-channel 0 (amp))   ; set channel values directly on
      (.SetValue freq-channel 0 (freq)) ; CsoundMYFLTArray's
      (recur (.PerformKsmps c))))
  (.Stop c))

