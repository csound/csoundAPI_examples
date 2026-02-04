/*
Copyright (C) 2024 Victor Lazzarini
Adapted for go by François Pinot

API Examples: control channels

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
	"fmt"
	"os"

	"github.com/fggp/go-csnd7"
)

var code string = `
0dbfs = 1        
instr 1  
  p:k = chnget("pitch")
  a1 = expon(p4, p3, 0.001)
  a2 = poscil(a1,cpsmidinn(p5)*p)
  out(a2)   
endin
`

func main() {
	// Create the Csound engine instance
	cs := csnd7.Create(nil, "")
	res := csnd7.CSOUND_SUCCESS
	// Set options checking for any errors
	for _, opt := range os.Args[1:] {
		res += cs.SetOption(opt)
	}
	if res == csnd7.CSOUND_SUCCESS {
		// Compile code from string, synchronously
		res = cs.CompileOrc(code, false)
		if res == csnd7.CSOUND_SUCCESS {
			dur := csnd7.MYFLT(5.0)
			pitch, incr := csnd7.MYFLT(1.0), 1.0/(dur*cs.Kr())
			// Start engine
			res = cs.Start()
			if res == csnd7.CSOUND_SUCCESS {
				// send realtine events, synchronously
				evt := fmt.Sprintf("i1 0 %f 0.1 60\n", dur)
				cs.EventString(evt, false)
				// compute audio blocks
				for {
					cs.SetControlChannel("pitch", pitch)
					if cs.PerformKsmps() {
						break
					}
					pitch += incr
					if pitch > 2 {
						cs.EventString("e 0", false)
					}
				}
			}
		}
	}
	os.Exit(res)
}
