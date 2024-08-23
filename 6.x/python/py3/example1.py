# Example 1 - Simple Compilation with Csound
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
#
# This example is a barebones example for creating an instance of Csound, 
# compiling a pre-existing CSD, calling Perform to run Csound to completion,
# then Stop and exit.  

# The first thing we do is import the csnd6 module, which is the module 
# containing the Python interface to the Csound API.

import ctcsound

c = ctcsound.Csound()     # Create an instance of the Csound object
args = ["dummy", "../test1.csd"]
c.compile_(args)          # Compile a pre-defined test1.csd file
c.perform()               # This call runs Csound to completion
c.stop()                  # At this point, Csound is already stopped, but this call is here
                          # as it is something that you would generally call in real-world 
                          # contexts 

