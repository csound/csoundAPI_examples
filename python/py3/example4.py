# Example 4 - Using Csound's Performance Thread 
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# In this example, we use a CsoundPerformanceThread to run Csound in 
# a native thread.  Using a native thread is important to get the best
# runtime performance for the audio engine.  It is especially important
# for languages such as Python that do not have true native threads
# and that use a Global Interpreter Lock. CsoundPerformanceThread has
# some convenient methods for handling events, but does not have
# features for doing regular processing at block boundaries.  In general,
# use CsoundPerformanceThread when the only kinds of communication you
# are doing with Csound are through events, and not using channels.


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


c = ctcsound.Csound() # create an instance of Csound
c.setOption("-odac")  # Set option for Csound
c.compileOrc(orc)     # Compile Orchestra from String
c.readScore(sco)      # Read in Score from String
c.start()             # When compiling from strings, this call is necessary before doing any performing

t = ctcsound.CsoundPerformanceThread(c.csound())  # Create a new CsoundPerformanceThread, passing in the Csound object
t.play()              # starts the thread, which is now running separately from the main thread. This 
                      # call is asynchronous and will immediately return back here to continue code
                      # execution.
t.join()              # Join will wait for the other thread to complete. If we did not call Join(),
                      # after t.Play() returns we would immediate move to the next line, c.Stop(). 
                      # That would stop Csound without really giving it time to run. 

c.stop()              # stops Csound
c.cleanup()           # clean up Csound; this is useful if you're going to reuse a Csound instance
