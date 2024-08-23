require 'csnd6'
import 'csnd6.Csound'

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

# Our Score for our project
sco = "i1 0 1 0.5 8.00"

c = Csound.new    # create an instance of Csound
c.SetOption("-odac")  # Set option for Csound
c.CompileOrc(orc)     # Compile Orchestra from String

sco2 = ""
13.times { |i| sco2 << "i1 %g 0.25 0.5 8.%02g\n" % [i * 0.25, i] }
#puts sco2

vals = []
13.times do |i|
  vals << [1, i * 0.25, 0.25, 0.5, "8.%02g" % (rand(0..15))]
end
#puts vals.inspect

# convert list of lists into a list of strings
# then convert that list of strings into a single string
sco3 = vals.map! { |v| "i " + v.join(" ") }.join("\n")
puts sco3


#c.ReadScore(sco)      # Read in Score from pre-written String
#c.ReadScore(sco2)     # Read in Score from loop-generated String
c.ReadScore(sco3)     # Read in Score from loop-generated String

c.Start             # When compiling from strings, this call is necessary before doing any performing

# the following is our main performance loop
# We will perform one block of sound at a time 
# and continue to do so while it returns 0,
# which signifies to keep processing

while c.PerformKsmps == 0 do
  next
end

c.Stop
c.Cleanup


