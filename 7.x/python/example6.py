"""
  Copyright (C) 2024 Victor Lazzarini
  Adapted for Python by François Pinot

  API Examples: multi-threaded performance
  
  This file is part of Csound.

  The Csound Library is free software; you can redistribute it
  and/or modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  Csound is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Csound; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
  02111-1307 USA
"""
import ctcsound
import sys

code = """
0dbfs = 1
instr 1
icnt = 0
while icnt < 12 do
schedule 2,icnt*p3,p3*2,p4,cpsmidinn(icnt+p5)
icnt += 1
od
endin
instr 2
a1 expon p4,p3,0.001
a2 oscil a1,p5
   out a2 
endin
"""

# Create the Csound engine instance
csound = ctcsound.Csound()
# enforce realtime output and suppress messages 
csound.set_option("-o dac -dm0")
# Compile code from string
res = csound.compile_orc(code)
if res == ctcsound.CSOUND_SUCCESS:
    evt = ""
    csound_performance_thread = ctcsound.CsoundPerformanceThread(csound.csound())
    # Start engine
    res = csound.start()
    # start performance thread
    if res == ctcsound.CSOUND_SUCCESS:
        csound_performance_thread.play()
    while csound_performance_thread.is_running():
        # prompt for input
        csound.message("Csound>")
        # take in event from stdin, 
        #  use event e <t> to finish after t secs 
        evt = input()
        # send in event asynchronously
        csound.event_string(evt, async_=True)
        # exit loop if requested
        if evt[0] == "e":
            break
    # Join thread an wait for it to finish
    csound_performance_thread.join()
sys.exit()
