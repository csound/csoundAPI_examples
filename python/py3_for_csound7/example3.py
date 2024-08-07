# Example 3 - Using our own performance loop
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
# Adapted for Csound 7.00 by François Pinot, August 2024
#
# In this example, we use a while loop to perform Csound one audio block at a time.
# This technique is important to know as it will allow us to do further processing
# safely at block boundaries.  We will explore the technique further in later examples.

import ctcsound

# Our Orchestra for our project
orc = """
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
aout vco2 0.5, 440
outs aout, aout
endin"""

# Our Score for our project
sco = "i1 0 1"


cs = ctcsound.Csound() # create an instance of Csound
cs.set_option("-odac") # Set option for Csound
cs.compile_orc(orc)    # Compile Orchestra from String
cs.event_string(sco)   # Read in Score from String
cs.start()             # When compiling from strings, this call is necessary
                       # before doing any performing

# The following is our main performance loop. We will perform one block of
# sound at a time and continue to do so while it returns 0, which signifies
# to keep processing.  We will explore this loop technique in further examples.

while (cs.perform_ksmps() == 0):
  pass
