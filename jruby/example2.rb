# Example 2 - Compilation with Csound without CSD
#
# This example ...

require 'csnd6'
import 'csnd6.Csound'

orc = "
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
aout vco2 0.5, 440
outs aout, aout
endin"

sco = "i1 0 1"

c = Csound.new
c.SetOption("-odac")
c.CompileOrc(orc)
c.ReadScore(sco)
c.Start
c.Perform
c.Stop
c.Cleanup

