# Example 9 - More efficient Channel Communications
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
#
# This example continues on from Example 9 and just refactors the
# creation and setup of CsoundMYFLTArray's into a createChannel() 
# function.  This example illustrates some natural progression that
# might occur in your own API-based projects, and how you might 
# simplify your own code.


import ctcsound
from random import randint, random
import sys

class RandomLine(object):
    def __init__(self, base, range):
        self.curVal = 0.0
        self.reset()
        self.base = base
        self.range = range

    def reset(self):
        self.dur = randint(256,512) 
        self.end = random() 
        self.slope = (self.end - self.curVal) / self.dur

    def getValue(self):
        self.dur -= 1
        if(self.dur < 0):
            self.reset()
        retVal = self.curVal
        self.curVal += self.slope
        return self.base + (self.range * retVal)


def createChannel(csound, channelName):
    "Creates a Csound Channel and returns it as a numpy ndarray"
    chn, err = csound.channelPtr(channelName, 
        ctcsound.CSOUND_CONTROL_CHANNEL | ctcsound.CSOUND_INPUT_CHANNEL)
    if err:
    	sys.exit(1)
    return chn

###############################

# Our Orchestra for our project
orc = """
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
kamp chnget "amp"
kfreq chnget "freq"
printk 0.5, kamp
printk 0.5, kfreq
aout vco2 kamp, kfreq
aout moogladder aout, 2000, 0.25
outs aout, aout
endin"""

c = ctcsound.Csound()    # create an instance of Csound
c.setOption("-odac")  # Set option for Csound
c.setOption("-m7")  # Set option for Csound
c.compileOrc(orc)     # Compile Orchestra from String

sco = "i1 0 60\n"

c.readScore(sco)     # Read in Score generated from notes 

c.start()             # When compiling from strings, this call is necessary before doing any performing

ampChannel = createChannel(c, "amp")   # uses utility method to create a channel and get an ndarray
freqChannel = createChannel(c, "freq")

amp = RandomLine(.4, .2)
freq = RandomLine(400, 80)

ampChannel[0] = amp.getValue()
freqChannel[0] = freq.getValue()

while (c.performKsmps() == 0):
    ampChannel[0] = amp.getValue()
    freqChannel[0] = freq.getValue()

c.stop()



