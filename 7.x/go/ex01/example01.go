/*
Copyright (C) 2024 Victor Lazzarini
Adapted for go by François Pinot

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
*/
package main

import (
	"os"

	"github.com/fggp/go-csnd7"
)

func main() {
	// Create the Csound engine instance
	cs := csnd7.Create(nil, "")
	// Compile code from command-line arguments
	res := cs.Compile(os.Args)
	if res == csnd7.CSOUND_SUCCESS {
		// Start engine
		if res = cs.Start(); res == csnd7.CSOUND_SUCCESS {
			// compute audio blocks
			for !cs.PerformKsmps() {
			}
		}
	}
	os.Exit(0)
}
