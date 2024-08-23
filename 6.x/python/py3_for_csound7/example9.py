# Example 9 - More efficient Channel Communications
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
# Adapted for Csound 7.00 by François Pinot, August 2024
#
# This example continues on from Example 9 and just refactors the
# creation and setup of CsoundMYFLTArray's into a create_channel() 
# function.  This example illustrates some natural progression that
# might occur in your own API-based projects, and how you might 
# simplify your own code.


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


def create_channel(csound, channelName):
    "Creates a Csound Channel and returns it as a numpy ndarray"
    chn, err = csound.channel_ptr(channelName, 
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

cs = ctcsound.Csound()    # create an instance of Csound
cs.set_option("-odac")    # Set option for Csound
cs.set_option("-m7")      # Set option for Csound
cs.compile_orc(orc)       # Compile Orchestra from String

sco = "i1 0 60\n"

cs.event_string(sco)     # Read in Score generated from notes 

cs.start()               # When compiling from strings, this call is necessary
                         # before doing any performing

amp_channel = create_channel(cs, "amp")    # uses utility method to create
                                          # a channel and get an ndarray
freq_channel = create_channel(cs, "freq")

amp = RandomLine(.4, .2)
freq = RandomLine(400, 80)

amp_channel[0] = amp.get_value()
freq_channel[0] = freq.get_value()

while (cs.perform_ksmps() == 0):
    amp_channel[0] = amp.get_value()
    freq_channel[0] = freq.get_value()
