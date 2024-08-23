# Example 3 - Using our own performance loop
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# In this example, we use a while loop to perform Csound one audio block at a time.
# This technique is important to know as it will allow us to do further processing
# safely at block boundaries.  We will explore the technique further in later examples.

import csnd6

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


c = csnd6.Csound()    # create an instance of Csound
c.SetOption("-odac")  # Set option for Csound
c.CompileOrc(orc)     # Compile Orchestra from String
c.ReadScore(sco)      # Read in Score from String
c.Start()             # When compiling from strings, this call is necessary before doing any performing

# The following is our main performance loop. We will perform one block of sound at a time 
# and continue to do so while it returns 0, which signifies to keep processing.  We will
# explore this loop technique in further examples.

while (c.PerformKsmps() == 0):
  pass

c.Stop()


