/*
Copyright (C) 2024 Victor Lazzarini
Adapted for go by François Pinot

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
*/
package main

import (
	"bufio"
	"os"

	"github.com/fggp/go-csnd7"
)

var code string = `
0dbfs = 1
instr 1
  cnt:i = 0
  while cnt < 12 do
  schedule(2, cnt*p3, p3*2, p4,cpsmidinn(cnt+p5))
  cnt += 1
od
endin
instr 2
  a1 = expon(p4, p3, 0.001)
  a2 = poscil(a1, p5)
  out(a2)
endin
`

func process(cs csnd7.CSOUND, done chan bool) {
	for !cs.PerformKsmps() {
	}
	done <- true
}

func main() {
	// Create the Csound engine instance
	cs := csnd7.Create(nil, "")
	// enforce realtime output and suppress messages
	cs.SetOption("-o dac -dm0")
	// Compile code from string
	res := cs.CompileOrc(code, false)
	if res == csnd7.CSOUND_SUCCESS {
		// Start engine
		res = cs.Start()
		if res == csnd7.CSOUND_SUCCESS {
			// Start performance goroutine
			done := make(chan bool)
			go process(cs, done)
			scanner := bufio.NewScanner(os.Stdin)
			evt := ""
			for {
				// prompt for input
				cs.Message("Csound>")
				// take in event from stdin,
				// use event e <t> to finish after t secs
				scanner.Scan()
				evt = scanner.Text()
				// send in event asynchronously
				cs.EventString(evt, true)
				// exit loop if requested
				if evt[0] == 'e' {
					break
				}
			}
			// Wait for performance goroutine to finish
			<-done
		}
	}
	os.Exit(res)
}
