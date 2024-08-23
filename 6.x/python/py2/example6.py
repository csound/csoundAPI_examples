# Example 6 - Generating Score
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# This example continues on from Example 5, rewriting the example using
# a Class called Note. The note example has its __str__ method implemented
# to generate a well-formatted Csound SCO note.  
#
# This example also shows how a list of notes could be used multiple times.
# The first loop through we use the notes as-is, and during the second time
# we generate the notes again with the same properties except we alter the 
# fifth p-field up 4 semitones. 
#
# Note: Altering a Notes values like this is alright for this example, but 
# it is a destructive edit.  Real world code might make copies of Notes or 
# alter the score generation to maintain the original values. 


import csnd6
from random import randint

def midi2pch(num):
    "Convert MIDI Note Numbers to Csound PCH format"
    return "%d.%02g" % (3 + (num / 12), num % 12)

class Note(object):
    def __init__(self, *args):
        self.pfields = list(args)

    def __str__(self):
        retVal = "i"
        for i in range(len(self.pfields)):
            if(i == 4):
                retVal += " " + midi2pch(self.pfields[i])
            else:
                retVal += " " + str(self.pfields[i])
        return retVal


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

c = csnd6.Csound()    # create an instance of Csound
c.SetOption("-odac")  # Set option for Csound
c.CompileOrc(orc)     # Compile Orchestra from String


notes = []           #initialize a list to hold lists of values 
for i in range(13): #populate that list
    notes.append( Note(1, i * .25, .25, 0.5, randint(60,75)) )

# now convert list of Note objects to string
sco = ""
for n in notes:
    sco += "%s\n"%n # this implicitly calls the __str__ method on the Note Class

# generate notes again transposed a Major 3rd up
for n in notes:
    n.pfields[4] += 4
    n.pfields[1] += .125
    sco += "%s\n"%n 

print sco

c.ReadScore(sco)     # Read in Score generated from notes 

c.Start()             # When compiling from strings, this call is necessary before doing any performing

# The following is our main performance loop. We will perform one block of sound at a time 
# and continue to do so while it returns 0, which signifies to keep processing.  

while (c.PerformKsmps() == 0):
  pass

c.Stop()




