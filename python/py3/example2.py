# Example 2 - Compilation with Csound without CSD
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# In this example, we move from using an external CSD file to 
# embedding our Csound ORC and SCO code within our Python project.
# Besides allowing encapsulating the code within the same file,
# using the CompileOrc() and CompileSco() API calls is useful when
# the SCO or ORC are generated, or perhaps coming from another 
# source, such as from a database or network.

import ctcsound

# Defining our Csound ORC code within a triple-quoted, multiline String
orc = """
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
aout vco2 0.5, 440
outs aout, aout
endin"""


# Defining our Csound SCO code 
sco = "i1 0 1"

c = ctcsound.Csound()
c.setOption("-odac")  # Using SetOption() to configure Csound
                      # Note: use only one commandline flag at a time

c.compileOrc(orc)     # Compile the Csound Orchestra string
c.readScore(sco)      # Compile the Csound SCO String
c.start()             # When compiling from strings, this call is necessary
                      # before doing any performing
c.perform()           # Run Csound to completion
c.stop()

