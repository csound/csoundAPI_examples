"""
  Copyright (C) 2024 Victor Lazzarini
  Adapted for Python by François Pinot

  API Examples: realtime events
  
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
 a1 expon p4,p3,0.001
 a2 oscil a1,cpsmidinn(p5)
    out a2
endin
"""

# Create the Csound engine instance
csound = ctcsound.Csound()
res = ctcsound.CSOUND_SUCCESS
# Set options checking for any errors
for opt in sys.argv[1:]:
    res += csound.set_option(opt)
if res == ctcsound.CSOUND_SUCCESS:
    # Compile code from string, synchronously
    res = csound.compile_orc(code)
    if res == ctcsound.CSOUND_SUCCESS:
        evt = ""
        # Start engine
        res = csound.start()
        # send realtine events, synchronously
        for i in range(13):
            evt += f'i1 {i*0.25:f} 0.3 0.1 {i+60:d}\n'
        evt += f'e {i*0.3:f}\n'
        csound.event_string(evt)
        # compute audio blocks
        while res == ctcsound.CSOUND_SUCCESS:
            res = csound.perform_ksmps()
sys.exit()
