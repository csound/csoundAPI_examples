/*
Copyright (C) 2024 Victor Lazzarini
Adapted for go by François Pinot

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
*/
package main

import (
	"math"
	"os"

	"github.com/fggp/go-csnd7"
)

var code string = `
0dbfs = 1
instr 1
  a1 = expon(p4, p3, 0.001)
  a2 = poscil(a1, p5)
  out(a2)
endin
cnt:i = 0
while cnt <= 12 do
  schedule(1, cnt*0.25, 0.3, 0.5, cpsmidinn(60+cnt))
  cnt += 1
od
eventi("e", cnt*0.25)
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
			rms := csnd7.MYFLT(0.0)
			nsmps := int(cs.Ksmps())
			// Start engine
			cs.Start()
			// get spout pointer
			spout := cs.Spout()
			// compute audio blocks
			for !cs.PerformKsmps() {
				// compute output power rms
				for i := range nsmps {
					rms = 0.01*rms + 0.99*spout[i]*spout[i]
				}
				// print output amp rms
				cs.Message("rms: \t%f\n", math.Sqrt(float64(rms)))
			}
		}
	}
	os.Exit(res)
}
