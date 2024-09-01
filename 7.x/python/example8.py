# Example 8 - More efficient Channel Communications
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
# Adapted for Csound 7.00 by François Pinot, August 2024
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
        self.cur_val = 0.0
        self.reset()
        self.base = base
        self.range = range

    def reset(self):
        self.dur = randint(256,512) 
        self.end = random() 
        self.slope = (self.end - self.cur_val) / self.dur

    def get_value(self):
        self.dur -= 1
        if(self.dur < 0):
            self.reset()
        ret_val = self.cur_val
        self.cur_val += self.slope
        return self.base + (self.range * ret_val)

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

cs = ctcsound.Csound()    # create an instance of Csound
cs.set_option("-odac")    # Set option for Csound
cs.set_option("-m7")      # Set option for Csound
cs.compile_orc(orc)       # Compile Orchestra from String

sco = "i1 0 60\n"

cs.event_string(sco)     # Read in Score generated from notes 

cs.start()               # When compiling from strings, this call is
                         # necessary before doing any performing

# the following calls get the Channel Pointer retreived from Csound
amp_channel, err = cs.channel_ptr("amp",
	ctcsound.CSOUND_CONTROL_CHANNEL | ctcsound.CSOUND_INPUT_CHANNEL)
if err:
	sys.exit(1)

freq_channel, err = cs.channel_ptr("freq",
	ctcsound.CSOUND_CONTROL_CHANNEL | ctcsound.CSOUND_INPUT_CHANNEL)
if err:
	sys.exit(1)

amp = RandomLine(.4, .2)
freq = RandomLine(400, 80)

amp_channel[0]  = amp.get_value()
freq_channel[0] = freq.get_value()

#print(amp.get_value())
#print(freq.get_value())

while (cs.perform_ksmps() == 0):
    amp_channel[0] = amp.get_value()
    freq_channel[0] = freq.get_value()
