# Example 13 - Using ctcsound (works with Python 2 or 3) and recompiling
# Author: Victor Lazzarini <victor.lazzarini@nuim.ie>
# 2017.04.16
# Adapted for Csound 7.00 by François Pinot, August 2024
#
# This example shows how to compile and start an instrument, run it then
# re-compile instr0 (global code) and start a new instance of instr 1


import ctcsound          # import the ctcsound module
cs = ctcsound.Csound()   # start an instance of Csound
cs.set_option("-odac")   # add the option for realtime audio
cs.compile_orc('''       
gi1 init 1
instr 1
a1 oscili 0dbfs, A4*gi1
out a1
endin
schedule(1,0,1)
''')                    # compile the instrument
cs.start()              # start Csound
i = 0  
while i < 4410:         # run Csound for 1 sec (ksmps=10, sr=44100)
    cs.perform_ksmps()        
    i += 1
cs.compile_orc('''   
gi1 = 2
schedule(1,0,2)
''')                    # compile new instr 0
i = 0
while i < 4410:         # carry on running Csound for another second
    cs.perform_ksmps()
    i += 1
