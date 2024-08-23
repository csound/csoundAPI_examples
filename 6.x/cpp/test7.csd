<CsoundSynthesizer>
<CsOptions>
-+rtaudio=jack -odac
</CsOptions>
<CsInstruments>
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1
    kFreq chnget "freq"
    aOut oscili .5, kFreq
    outs aOut, aOut
endin

</CsInstruments>
<CsScore>
i1 0 100
e
</CsScore>
</CsoundSynthesizer>