/*
Copyright (C) 2024 Victor Lazzarini
Adapted for go by François Pinot

API Examples: compiling code on-the-fly

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

var code string = `
0dbfs = 1
instr 1
  a1 = expon(p4, p3, 0.001)
  a2 = poscil(a1, p5)
  out(a2)
endin
cnt:i = 0
while cnt <= 12 do
  schedule(1, cnt*0.25, 0.3, 0.1, cpsmidinn(60+cnt))
  cnt += 1
od
eventi("e", 10)
`

var perf string = `
cnt:i = 0
while cnt <= 12 do
  schedule(1, cnt*0.25, 0.3, 0.1, cpsmidinn(60+cnt))
  cnt += 1
od
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
			time := csnd7.MYFLT(0.0)
			incr := 1.0 / cs.Kr()
			// Start engine
			res = cs.Start()
			if res == csnd7.CSOUND_SUCCESS {
				// compute audio blocks
				for {
					if cs.PerformKsmps() {
						break
					}
					// count time
					time += incr
					// after 2.5 seconds
					if time > 2.5 {
						// compile new code, synchronously
						cs.CompileOrc(perf, false)
						// reset time
						time -= 2.5
					}
				}
			}
		}
	}
	os.Exit(res)
}
