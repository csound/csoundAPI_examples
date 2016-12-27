; Example 10 - More efficient Channel Communications
; Author: Steven Yi <stevenyi@gmail.com>
; 2013.10.28
;
; This example continues on from Example 10 and introduces a 
; create-channel-updater function. This function returns a function
; that will update a value in a Csound channel using a given update
; function. The function that is return closes over the generated
; CsoundMYFLTArray and update-func so that it update state when
; the returned function is called.
;
; This example continues the illustration of a progression of 
; a project.  Note that the process has changed a little bit
; where we now create a number channel-updater functions and
; store them in a list.  The list is then iterated through for
; updating the channel with the latest values.  In a real-world
; project, this kind of scenario occurs when there are n-number of 
; items to update channels and one wants to have a flexible number
; that may even change dynamically at runtime.


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

(defn create-channel-updater [csound channel-name update-func]
  "Returns a function that will update a Csound channel using the given update-func"
 (let [chn (create-channel csound channel-name)]
   (println "Creating channel updater" channel-name update-func)
   (fn []
      (println "Setting value...")
      (.SetValue chn 0 (update-func)))))

; Defining our Csound ORC code within a multiline String
(def orc "
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
kamp chnget \"amp\"
kfreq chnget \"freq\"
kres chnget \"resonance\"
printk 0.5, kamp
printk 0.5, kfreq
printk 0.5, kres
aout vco2 kamp, kfreq
aout moogladder aout, 2000, kres
outs aout, aout
endin")


(def sco "i1 0 60")

; define Csound input Control Channel type
; uses .swigValue to get the int values 
(let [c (Csound.)
      updaters (map (fn [[a b]] 
                      (create-channel-updater c a b))
                    [["amp" (random-line 0.4 0.2)]
                     ["freq" (random-line 400.0 80.0)]
                     ["resonance" (random-line 0.4 0.3)]])] 
  (.SetOption c "-odac") ; Using SetOption() to configure Csound
  (.SetOption c "-m7") ; Using SetOption() to configure Csound
  ; Note: use only one commandline flag at a time
  (.CompileOrc c orc)    ; Compile the Csound Orchestra String
  (.ReadScore c sco)     ; Read in Score from pre-written String 
  (.Start c)             ; When compiling from strings, this call is necessary before doing any performing 

  ; Main Performance Loop 
  (loop [retval 0]
    (when (zero? retval)
      ; run updaters using loop
      (loop [[a & b] updaters]
       (when a
         (a)
         (recur b)))
      (recur (.PerformKsmps c))))

  (.Stop c))


