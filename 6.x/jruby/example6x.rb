# Example 6 - Generating Score
#
# This version of example 6 enhances the Note class with getter/setter
# methods for each p-field using ORC-style names. This is done using the Ruby
# meta-programming technique of creating new methods on the fly based on the
# pattern of the name. In this case the patterns are "pN" and "pN=" where N
# is some integer. The new methods are cached and have the same lifetime as
# the instance they belong to.
#
# For example:
#
#   # create now note instance
#   n = Note.new(1, i * 0.25, 0.25, 0.5, rand(60..75))
#
#   # change the values of p4 and p5
#   n.p2 += 0.125       # time offset for arpeggiation
#   n.p5 += 4           # transposition
#
#   # print the value of p3
#   puts n.p3
#
require 'csnd6'
import 'csnd6.Csound'

def midi2pch(num)
  "%d.%02g" % [3 + (num / 12), num % 12]
end

# helper class for matching pfields
# will match "p1" or "p1="
class PMatcher
  attr_reader :index
  attr_reader :set
  
  def initialize(method_sym)
    @set = false
    if method_sym.to_s =~ /^p(\d+)(=?)$/
      @attribute = $1.to_sym      # pfield number (symbol)
      @index = $1.to_i            # pfield number (integer)
      @set = true if !$2.empty?   # flag: true if this is a setter ("p2=")
    end
  end
  
  def match?
    @attribute != nil
  end
end

class Note
  # accept a variable number of pfield arguments, store in internal array
  def initialize(*pfields)
    @pfields = pfields
  end

  # override: standard string conversion
  def to_s
    s = ["i"]
    @pfields.each_with_index do |p,i|
      s << (i == 4 ? midi2pch(p) : p.to_s)
    end
    s.join(" ")
  end

  # overide: look for method calls that look like "p1" or "p1="
  def method_missing(method_sym, *arguments, &block)
    match = PMatcher.new(method_sym)
    if match.match?
      i = match.index - 1                 # pfield names are 1-based, but the internal array i 0-based
      if match.set
        # setter
        self.class.class_eval <<-EOF
          def p#{match.index}=(val)
            @pfields[#{i}] = val
          end
        EOF
        @pfields[i] = arguments.first
      else
        # getter
        self.class.class_eval <<-EOF
          def p#{match.index}
            @pfields[#{i}]
          end
        EOF
        @pfields[i]
      end
    else
      super
    end
  end

  # override: indicate that we understand methods that look like "p1" and "p1="
  def self.respond_to?(method_sym, include_private=false)
    PMatcher.new(method_sym).match? ? true : super
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

# make an array of Note objects
notes = [] 
13.times do |i|
  notes << Note.new(1, i * 0.25, 0.25, 0.5, rand(60..75))
end

# convert array of Note objects to a string
sco = notes.join("\n")
#puts sco

# add copy of notes transposed up four semitones
notes.each do |n|
  n.p2 += 0.125       # time offset for arpeggiation
  n.p5 += 4           # transposition 
  sco << "\n#{n}"     # convert to string and append to score
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

