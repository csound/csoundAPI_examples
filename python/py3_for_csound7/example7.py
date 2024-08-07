# Example 7 - Communicating continuous values with Csound's Channel System
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
# Adapted for Csound 7.00 by François Pinot, August 2024
#
# This example introduces using Csound's Channel System to communicate 
# continuous control data (k-rate) from a host program to Csound. The 
# first thing to note is the RandomLine class. It takes in a base value
# and a range in which to vary randomly.  The reset functions calculates
# a new random target value (self.end), a random duration in which to 
# run (self.dur, expressed as # of audio blocks to last in duration), and
# calculates the increment value to apply to the current value per audio-block.
# When the target is met, the Randomline will reset itself to a new target
# value and duration.
# 
# In this example, we use two RandomLine objects, one for amplitude and 
# another for frequency.  We start a Csound instrument instance that reads
# from two channels using the chnget opcode. In turn, we update the values
# to the channel from the host program.  In this case, because we want to 
# keep our values generating in sync with the audio engine, we use a 
# while-loop instead of a CsoundPerformanceThread. To update the channel,
# we call the SetChannel method on the Csound object, passing a channel name
# and value.  Note: The get_value method on the RandomLine not only gets
# us the current value, but also advances the internal state by the increment
# and by decrementing the duration.

import ctcsound
from random import randint, random

class RandomLine(object):
    def __init__(self, base, range_):
        self.cur_val = 0.0
        self.reset()
        self.base = base
        self.range_ = range_

    def reset(self):
        self.dur = randint(256,512) 
        self.end = random() 
        self.increment = (self.end - self.cur_val) / self.dur

    def get_value(self):
        self.dur -= 1
        if(self.dur < 0):
            self.reset()
        ret_val = self.cur_val
        self.cur_val += self.increment
        return self.base + (self.range_ * ret_val)

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

cs = ctcsound.Csound()     # create an instance of Csound
cs.set_option("-odac")     # Set option for Csound
cs.set_option("-m7")       # Set option for Csound
cs.compile_orc(orc)        # Compile Orchestra from String

sco = "i1 0 60\n"

cs.event_string(sco) # Read in Score generated from notes 
cs.start(   )        # When compiling from strings, this call is necessary
                     # before doing any performing


# The following is our main performance loop. We will perform one block of
# sound at a time and continue to do so while it returns 0, which signifies
# to keep processing.  


amp = RandomLine(.4, .2)    # create RandomLine for use with Amplitude
freq = RandomLine(400, 80)  # create RandomLine for use with Frequency 

cs.set_control_channel("amp", amp.get_value())   # Initialize channel value
                                                 # before running Csound
cs.set_control_channel("freq", freq.get_value()) # Initialize channel value
                                                 # before running Csound

print(amp.get_value())
print(freq.get_value())

while (cs.perform_ksmps() == 0):
    cs.set_control_channel("amp", amp.get_value())   # update channel value 
    cs.set_control_channel("freq", freq.get_value()) # update channel value 
