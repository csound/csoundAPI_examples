 <CsoundSynthesizer>
<CsOptions>
-odac
</CsOptions>
<CsInstruments>

0dbfs = 1
instr 1
 a1 expon p4,p3,0.001
 a2 oscil a1, p5
    out a2
endin
icnt = 0
while icnt <= 12 do
 schedule 1, icnt*0.25, 0.3, 0.1, cpsmidinn(60+icnt)
 icnt += 1
od
event_i "e", 0, icnt*0.3

</CsInstruments>
</CsoundSynthesizer>

