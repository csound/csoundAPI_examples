# Example 2 - Compilation with Csound without CSD
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
# Adapted for Csound 7.00 by François Pinot, August 2024
#
# In this example, we move from using an external CSD file to 
# embedding our Csound ORC and SCO code within our Python project.
# Besides allowing encapsulating the code within the same file,
# using the CompileOrc() and CompileSco() API calls is useful when
# the SCO or ORC are generated, or perhaps coming from another 
# source, such as from a database or network.

import ctcsound

def perform(cs):
    while True:
        finished = cs.perform_ksmps()
        if finished:
            break

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

cs = ctcsound.Csound()
cs.set_option("-odac")  # Using set_option() to configure Csound
                        # Note: use only one commandline flag at a time

cs.compile_orc(orc)     # Compile the Csound Orchestra string
cs.event_string(sco)    # Compile the Csound SCO String
cs.start()              # When compiling from strings, this call is necessary
                        # before doing any performing
perform(cs)             # Run Csound to completion
