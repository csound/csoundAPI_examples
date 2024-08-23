# Example 4 - Using Csound's Performance Thread 
#
# In this example, we use Csound's Performance Thread to run 
# Csound in a native thread.  This...

require 'csnd6'
import 'csnd6.Csound'
import 'csnd6.CsoundPerformanceThread'
#import 'csnd6.CsoundThreadLock'

# Our Orchestra for our project
orc = "
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
aout vco2 0.25, 440
outs aout, aout
endin"

# Our Score for our project
sco = "i1 0 1"

c = Csound.new        # create an instance of Csound
c.SetOption("-odac")  # Set option for Csound
c.CompileOrc(orc)     # Compile Orchestra from String
c.ReadScore(sco)      # Read in Score from String
c.Start               # When compiling from strings, this call is necessary before doing any performing

t = CsoundPerformanceThread.new(c)  # Create a new CsoundPerformanceThread, passing in the Csound object
t.Play                # starts the thread, which is now running separately from the main thread 
t.Join                # Join will wait for the other thread to complete
t.Stop

c.Stop                # stops Csound
c.Cleanup             # clean up Csound


