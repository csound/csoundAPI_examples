# Example 1 - Simple Compilation with Csound
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# Adapted for Python 3 by François Pinot, July 2021
# Adapted for Csound 7.00 by François Pinot, August 2024
#
# This example is a barebones example for creating an instance of Csound, 
# compiling a pre-existing CSD, calling Perform to run Csound to completion,
# then Stop and exit.  

# The first thing we do is import the csnd6 module, which is the module 
# containing the Python interface to the Csound API.

import ctcsound

def perform(cs):
    while True:
        finished = cs.perform_ksmps()
        if finished:
            break
    
cs = ctcsound.Csound()            # Create an instance of the Csound object
args = ["dummy", "test.csd"]
cs.compile_(args)                 # Compile a pre-defined test1.csd file
cs.start()
perform(cs)                       # This call runs Csound to completion
