<CsoundSynthesizer>
<CsOptions>
-+rtaudio=alsa -odac:hw:0,0
</CsOptions>
<CsInstruments>
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1
k1 oscil 1000, 1
SText sprintfk "number(\"%d\")", k1
chnset SText, "stringChannel"
endin

</CsInstruments>
<CsScore>
i1 0 2
e
</CsScore>
</CsoundSynthesizer>