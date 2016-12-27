; Example 8 - More efficient Channel Communications
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; This example builds on Example 7 by replacing the calls to SetChannel
; with using GetChannelPtr. In the Csound API, using SetChannel and GetChannel
; is great for quick work, but ultimately it is slower than pre-fetching the
; actual channel pointer.  This is because Set/GetChannel operates by doing a 
; lookup of the Channel Pointer, then setting or getting the value.  This 
; happens on each call. The alternative is to use GetChannelPtr, which fetches
; the Channel Pointer and lets you directly set and get the value on the pointer.
;
; In C/C++/Objective-C, one can directly use MYFLT* to get/set values.  However,
; for wrapped languages such as Python, Java, and Lua, it is generally not possible
; to get/set the value on the pointer itself.  The Csound API for host languages 
; uses a special wrapper object called CsoundMYFLTArray, which will hold a reference
; to a MYFLT*.  The CsoundMYFLTArray in turn has convenience methods for setting
; and getting values. 
;
; The code below shows how to use the CsoundMYFLTArray in conjunction with GetChannelPtr
; to have a more optimized channel setting system.


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
(def ctrl-chan-type 
  (bit-or (.swigValue controlChannelType/CSOUND_INPUT_CHANNEL) 
          (.swigValue controlChannelType/CSOUND_CONTROL_CHANNEL)))

(let [c (Csound.)
      amp (random-line 0.4 0.2)
      freq (random-line 400.0 80.0)
      amp-channel (CsoundMYFLTArray. 1)   ; create CsoundMYFLTArray of size 1
      freq-channel (CsoundMYFLTArray. 1)] ; create CsoundMYFLTArray of size 1

  (.SetOption c "-odac") ; Using SetOption() to configure Csound
  ; Note: use only one commandline flag at a time
  (.CompileOrc c orc)    ; Compile the Csound Orchestra String
  (.ReadScore c sco)     ; Read in Score from pre-written String 
  (.Start c)             ; When compiling from strings, this call is necessary before doing any performing 


  ; the following calls store the Channel Pointer retreived from Csound into the
  ; CsoundMYFLTArray Objects
  (.GetChannelPtr c (.GetPtr amp-channel) "amp" ctrl-chan-type)
  (.GetChannelPtr c (.GetPtr freq-channel) "freq" ctrl-chan-type)

  ; The following is our main performance loop. We will perform one block of sound at a time 
  ; and continue to do so while it returns 0, which signifies to keep processing.  We will
  ; explore this loop technique in further examples. 

  (loop [retval 0]
    (when (zero? retval)
      (.SetValue amp-channel 0 (amp))   ; set channel values directly on
      (.SetValue freq-channel 0 (freq)) ; CsoundMYFLTArray's
      (recur (.PerformKsmps c))))
  (.Stop c))

