# Example 8 - More efficient Channel Communications
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
#
# This example builds on Example 7 by replacing the calls to SetChannel
# with using GetChannelPtr. In the Csound API, using SetChannel and GetChannel
# is great for quick work, but ultimately it is slower than pre-fetching the
# actual channel pointer.  This is because Set/GetChannel operates by doing a 
# lookup of the Channel Pointer, then setting or getting the value.  This 
# happens on each call. The alternative is to use GetChannelPtr, which fetches
# the Channel Pointer and lets you directly set and get the value on the pointer.
#
# In C/C++/Objective-C, one can directly use MYFLT* to get/set values.  However,
# for wrapped languages such as Python, Java, and Lua, it is generally not possible
# to get/set the value on the pointer itself.  The Csound API for host languages 
# uses a special wrapper object called CsoundMYFLTArray, which will hold a reference
# to a MYFLT*.  The CsoundMYFLTArray in turn has convenience methods for setting
# and getting values. 
#
# The code below shows how to use the CsoundMYFLTArray in conjunction with GetChannelPtr
# to have a more optimized channel setting system.

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

# the following calls get the Channel Pointer retreived from Csound
ampChannel, err = c.channelPtr("amp",
	ctcsound.CSOUND_CONTROL_CHANNEL | ctcsound.CSOUND_INPUT_CHANNEL)
if err:
	sys.exit(1)
freqChannel, err = c.channelPtr("freq",
	ctcsound.CSOUND_CONTROL_CHANNEL | ctcsound.CSOUND_INPUT_CHANNEL)
if err:
	sys.exit(1)

amp = RandomLine(.4, .2)
freq = RandomLine(400, 80)

ampChannel[0]  = amp.getValue()
freqChannel[0] = freq.getValue()

#print(amp.getValue())
#print(freq.getValue())

while (c.performKsmps() == 0):
    ampChannel[0] = amp.getValue()
    freqChannel[0] = freq.getValue()

c.stop()
