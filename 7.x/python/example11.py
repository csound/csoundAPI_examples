"""
  Copyright (C) 2026 Pierre Clisson

  API Examples: MIDI input and output through host callbacks

  Needs a ctcsound newer than Csound 7.0.0's (csound/csound#3587),
  which hands the MIDI callbacks a pointer to Csound's buffer.

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
import ctypes
import numpy as np
import sys
from collections import deque

if not hasattr(ctcsound.Csound, "set_external_midi_write_callback"):
    sys.exit("This example needs a ctcsound newer than Csound 7.0.0's")

code = """
0dbfs = 1
instr 1
 inote notnum
 ivel veloc
 a1 linsegr 0, 0.01, ivel/254, 0.1, 0
 a2 oscili a1, cpsmidinn(inote)
    out a2
 ; echo the note to MIDI out, a fifth up, for 0.2 seconds
    noteondur 1, inote+7, ivel, 0.2
endin
"""

# The notes the host plays: (time in seconds, MIDI message)
notes = []
for i, note in enumerate([60, 64, 67, 72]):
    notes.append((i*0.5, bytes([0x90, note, 100])))      # note on
    notes.append((i*0.5 + 0.4, bytes([0x80, note, 0])))  # note off
notes.sort()
end = notes[-1][0] + 0.5
# The host's MIDI inbox, for Csound to read, and outbox, from Csound
inbox = deque()
outbox = deque()

# Csound opens and closes MIDI input and output through the host. With no
# device, one function serves both here. A host would set a callback for
# each, open its device named by dev_name, and could keep a handle in
# user_data[0], which Csound hands back to the other callbacks
def midi_open(csound_p, user_data, dev_name):
    return 0

def midi_close(csound_p, user_data):
    return 0

# Csound asks for MIDI input when its buffer is empty, and asks again
# after each read until this returns 0. It trusts the count returned:
# write whole messages, at most size bytes, and never raise
def midi_read(csound_p, user_data, buffer, size):
    data = b""
    while inbox and len(data) + len(inbox[0]) <= size:
        data += inbox.popleft()
    ctypes.memmove(buffer, data, len(data))
    return len(data)

# Csound hands over each MIDI message it sends
def midi_write(csound_p, user_data, buffer, nbytes):
    # buffer is a ctypes pointer with no length: nbytes says how much of
    # it to view as a numpy array. The view is good for this call only,
    # so keep a copy (bytes(buffer[:nbytes]) makes one without numpy)
    outbox.append(np.ctypeslib.as_array(buffer, shape=(nbytes,)).tobytes())
    return 0

# Create the Csound engine instance
csound = ctcsound.Csound()
# Csound leaves MIDI to the host's callbacks
csound.set_host_midi_IO()
csound.set_external_midi_in_open_callback(midi_open)
csound.set_external_midi_read_callback(midi_read)
csound.set_external_midi_in_close_callback(midi_close)
csound.set_external_midi_out_open_callback(midi_open)
csound.set_external_midi_write_callback(midi_write)
csound.set_external_midi_out_close_callback(midi_close)
res = ctcsound.CSOUND_SUCCESS
# Set options checking for any errors, MIDI input and output on
for opt in ["-M0", "-Q0"] + sys.argv[1:]:
    res += csound.set_option(opt)
if res == ctcsound.CSOUND_SUCCESS:
    # Compile code from string, synchronously
    res = csound.compile_orc(code)
    if res == ctcsound.CSOUND_SUCCESS:
        # Start engine
        res = csound.start()
        sr = csound.sr()
        # compute audio blocks, handing Csound each note when it is due
        while res == ctcsound.CSOUND_SUCCESS:
            now = csound.current_time_samples() / sr
            while notes and notes[0][0] <= now:
                inbox.append(notes.pop(0)[1])
            if now >= end:
                break
            res = csound.perform_ksmps()
            # what Csound sent during that block
            while outbox:
                message = outbox.popleft()
                print("MIDI out:", message.hex(" ").upper(), flush=True)
sys.exit()
