# Example 5 - Generating Score
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
# Adapted for Csound 7.00 by François Pinot, August 2024
#
# In this example, we will look at three techniques for generating our Score. 
# 
# The first is one we have already seen, which is to just write out the score
# by hand as a String.
#
# Knowing that we pass strings into Csound to pass note events, we can also
# generate the string.  In the second example, sco2 starts as an empty string.
# Using a for-loop, we append to sco2 note strings using a string formatting
# string that has its replacement values replaced.  The replace values are 
# calculated using the i value, and the result is an ascending note line. 
#
# In the final example, we are going to generate a list of lists.  The top-level
# list represents our score as a whole, and each sub-list within it represents
# the data for a single note.  The main list is then processed in two ways: first,
# it processes each sub-list and joins the values together into a single note string;
# second, it joins each individual note string into a single, large score string,
# separated by newlines.  The end result is a sequence of 13 notes with random
# pitches.
#
# The final example represents a common pattern of development.  For systems that
# employ some event-based model of music, it is common to use some kind of data
# structure to represent events.  This may use some kind of common data structure
# like a list, or it may be represented by using a class and instances of that
# class. 
#
# Note, the three examples here are indicated with comments.  To listen to the examples,
# look for the lines that have c.ReadScore(sco) (lines 80-82), uncomment the one
# you want to hear, and comment out the others. 

import ctcsound
from random import randint

# Our Orchestra for our project
orc = """
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
ipch = cps2pch(p5, 12)
kenv linsegr 0, .05, 1, .05, .7, .4, 0
aout vco2 p4 * kenv, ipch 
aout moogladder aout, 2000, 0.25
outs aout, aout
endin"""

# Example 1 - Static Score
sco = "i1 0 1 0.5 8.00"

cs = ctcsound.Csound()  # create an instance of Csound
cs.set_option("-odac")  # Set option for Csound
cs.compile_orc(orc)     # Compile Orchestra from String

# Example 2 - Generating Score string with a loop
sco2 = ""
for i in range(13):
    sco2 += "i1 %g .25 0.5 8.%02g\n"%(i * .25,i)
#print(sco2)


# Example 3 - Generating Score using intermediate data structure (list of lists),
#             then converting to String.
vals = []           #initialize a list to hold lists of values 
for i in range(13): #populate that list
    vals.append([1, i * .25, .25, 0.5, "8.%02g"%(randint(0,15))])

# convert list of lists into a list of strings
vals = ["i" + " ".join(map(str,a)) for a in vals] 

# now convert that list of strings into a single string
sco3 = "\n".join(vals)
#print(vals)
#print(sco3)


cs.event_string(sco)      # Read in Score from pre-written String
#cs.event_string(sco2)     # Read in Score from loop-generated String
#cs.event_string(sco3)     # Read in Score from loop-generated String

cs.start()             # When compiling from strings, this call is necessary before doing any performing

# The following is our main performance loop. We will perform one block of sound at a time 
# and continue to do so while it returns 0, which signifies to keep processing.  

while (cs.perform_ksmps() == 0):
  pass
