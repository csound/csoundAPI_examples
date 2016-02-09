<CsoundSynthesizer>
<CsOptions>
-+rtaudio=jack -odac
</CsOptions>
<CsInstruments>
sr=44100
ksmps=32
nchnls=1
0dbfs=1

instr 1
a2 inch 1		;access input signal from processing loop
a1 oscil 1, 200

out a1*a2		;multiply output of iscil by incoming signal
endin

</CsInstruments>
<CsScore>
i1 0 100
e
</CsScore>
</CsoundSynthesizer>