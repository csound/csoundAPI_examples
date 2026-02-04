/*
Copyright (C) 2024 Victor Lazzarini
# Adapted for go by François Pinot

API Examples: sending signals to main input

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
  k1 = oscil(1, 0.7, 1)
  a1 = linen(p4, 0.1, p3, 0.1)
  a2 = poscil(a1, cpsmidinn(p5+k1))
  out(a2)
endin
fn:i = ftgen(1, 0, 8, 7, 0, 8, 0)
schedule(1, 0, 5, 0.5, 60)
eventi("e", 5)
`

func main() {
	/* Create the Csound engine instance */
	cs := csnd7.Create(nil, "")
	res := csnd7.CSOUND_SUCCESS
	/* Set options checking for any errors */
	for _, opt := range os.Args[1:] {
		res += cs.SetOption(opt)
	}
	if res == csnd7.CSOUND_SUCCESS {
		/* Compile code from string, synchronously */
		res = cs.CompileOrc(code, false)
		if res == csnd7.CSOUND_SUCCESS {
			/* Start engine */
			res = cs.Start()
			if res == csnd7.CSOUND_SUCCESS {
				/* get table and fill it */
				ftab, tlen := cs.Table(1)
				for i := range tlen {
					ftab[i] = csnd7.MYFLT(i)
				}
				/* compute audio blocks */
				for !cs.PerformKsmps() {
				}
			}
		}
	}
	os.Exit(res)
}
