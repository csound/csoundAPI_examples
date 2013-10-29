# Example 1 - Simple Compilation with Csound
# Author: Steven Yi <stevenyi@gmail.com>
# 2013.10.28
#
# This example is a barebones example for creating an instance of Csound, 
# compiling a pre-existing CSD, calling Perform to run Csound to completion,
# then Stop and exit.  

# The first thing we do is import the csnd6 module, which is the module 
# containing the Python interface to the Csound API.

import csnd6

c = csnd6.Csound()        # Create an instance of the Csound object
c.Compile('test1.csd')    # Compile a pre-defined test1.csd file
c.Perform()               # This call runs Csound to completion
c.Stop()                  # At this point, Csound is already stopped, but this call is here
                          # as it is something that you would generally call in real-world 
                          # contexts 

