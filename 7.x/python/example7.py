"""
  Copyright (C) 2024 Victor Lazzarini
  Adapted for Python by François Pinot

  API Examples: accessing main outputs
  
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
import math
import sys

code = """
0dbfs = 1
instr 1
 a1 expon p4,p3,0.001
 a2 oscil a1, p5
    out a2
endin
icnt = 0
while icnt <= 12 do
 schedule 1, icnt*0.25, 0.3, 0.5, cpsmidinn(60+icnt)
 icnt += 1
od
event_i "e", icnt*0.25
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
        rms = 0.0
        nsmps = int(csound.ksmps())
        # Start engine
        csound.start()
        # get spout pointer
        spout = csound.spout()
        # compute audio blocks
        while res == ctcsound.CSOUND_SUCCESS:
            res = csound.perform_ksmps()
            # compute output power rms
            out_squared = 0.99*spout*spout
            for i in range(nsmps):
                rms = 0.01*rms + out_squared[i]
            # print output amp rms
            csound.message(f'rms: \t{math.sqrt(rms):f}\n')
sys.exit()
