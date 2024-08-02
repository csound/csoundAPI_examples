# Example 10 - More efficient Channel Communications
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
# Adapted for Csound 7.00 by François Pinot, August 2024
#
# This example continues on from Example 10 and introduces a 
# ChannelUpdater object. The ChannelUpdater will create and 
# store a CsoundMYFLTArray that is wrapping a Csound Channel.
# Additionally, it will store and call an object that has a 
# get_value() method to update values in the channel when 
# update() is called. 
# 
# This example continues the illustration of a progression of 
# a project.  Note that the process has changed a little bit
# where we now create a number of ChannelUpdater objects and
# store them in a list.  The list is then iterated through for
# updating the channel with the latest values.  In a real-world
# project, this kind of scenario occurs when there are n-number of 
# items to update channels and one wants to have a flexible number
# that may even change dynamically at runtime.


import ctcsound
from random import randint, random
import sys

class RandomLine(object):
    def __init__(self, base, range_):
        self.cur_val = 0.0
        self.reset()
        self.base = base
        self.range_ = range_

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
        return self.base + (self.range_ * ret_val)


def create_channel(csound, channelName):
    chn, err = csound.channel_ptr(channelName, 
        ctcsound.CSOUND_CONTROL_CHANNEL | ctcsound.CSOUND_INPUT_CHANNEL)
    if err:
    	sys.exit(1)
    return chn

class ChannelUpdater(object):
    def __init__(self, csound, channelName, updater):
        self.updater = updater
        self.channel = create_channel(csound, channelName)

    def update(self):
        self.channel[0] = self.updater.get_value()

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
kres chnget "resonance"
printk 0.5, kamp
printk 0.5, kfreq
printk 0.5, kres
aout vco2 kamp, kfreq
aout moogladder aout, 2000, kres
outs aout, aout
endin"""

cs = ctcsound.Csound()    # create an instance of Csound
cs.set_option("-odac")    # Set option for Csound
cs.set_option("-m7")      # Set option for Csound
cs.compile_orc(orc)       # Compile Orchestra from String

sco = "i1 0 60\n"

cs.event_string(sco)     # Read in Score generated from notes 

cs.start()               # When compiling from strings, this call is necessary before doing any performing

# Create a set of ChannelUpdaters
channels = [ChannelUpdater(cs, "amp", RandomLine(.4, .2)),
            ChannelUpdater(cs, "freq", RandomLine(400, 80)),
            ChannelUpdater(cs, "resonance", RandomLine(0.4, .3))]

# Initialize all Channel Values
for chn in channels:
    chn.update()

while (cs.perform_ksmps() == 0):
    for chn in channels:   # update all channel values
        chn.update()
