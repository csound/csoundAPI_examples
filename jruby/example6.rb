# Example 6 - Generating Score
#
# This example continues on from Example 5, rewriting the example using
# a Class called Note. The note example has its __str__ method implemented
# to generate a well-formatted Csound SCO note.  
#
# This example also shows how a list of notes could be used multiple times.
# The first loop through we use the notes as-is, and during the second time
# we generate the notes again with the same properties except we alter the 
# fifth p-field up 4 semitones. 

require 'csnd6'
import 'csnd6.Csound'

def midi2pch(num)
  "%d.%02g" % [3 + (num / 12), num % 12]
end

class Note
  attr_reader :pfields

  def initialize(*pfields)
    @pfields = pfields
  end
  
  def to_s
    s = ["i"]
    @pfields.each_with_index do |p,i|
      s << (i == 4 ? midi2pch(p) : p.to_s)
    end
    s.join(" ")
  end
end

# Our Orchestra for our project
orc = "
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
endin"

c = Csound.new        # create an instance of Csound
c.SetOption("-odac")  # Set option for Csound
c.CompileOrc(orc)     # Compile Orchestra from String


notes = []            # initialize an array of Note objects 
13.times do |i|
  notes << Note.new(1, i * 0.25, 0.25, 0.5, rand(60..75))
end
#puts notes.inspect

# convert array of Note objects to a string
sco = notes.join("\n")

# add copy of notes a Major 3rd transposed up
notes.each do |n|
  n.pfields[1] += 0.125
  n.pfields[4] += 4
  sco << "\n#{n}" 
end
#puts sco

c.ReadScore(sco)      # Read in Score generated from notes 
c.Start               # When compiling from strings, this call is necessary before doing any performing

# the following is our main performance loop
# We will perform one block of sound at a time 
# and continue to do so while it returns 0,
# which signifies to keep processing

while c.PerformKsmps == 0 do
  next
end

c.Stop
c.Cleanup

