"""
  Copyright (C) 2024 Victor Lazzarini
  Adapted for Python by François Pinot

  API Examples: simple CLI frontend
  
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

# Create the Csound engine instance
csound = ctcsound.Csound()
# Compile code from command-line arguments
res = csound.compile_(sys.argv)
if res == ctcsound.CSOUND_SUCCESS:
    # Start engine
    res = csound.start()
    # compute audio blocks
    while res == ctcsound.CSOUND_SUCCESS:
        res = csound.perform_ksmps()
sys.exit()
