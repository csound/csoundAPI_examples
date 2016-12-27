// Example 2 - Compilation with Csound without CSD
// Adapted for golang by Francois Pinot <fggpinot@gmail.com>, 2013.11.15,
// from the original python example by Steven Yi <stevenyi@gmail.com>
//
// In this example, we move from using an external CSD file to
// embedding our Csound ORC and SCO code within our Python project.
// Besides allowing encapsulating the code within the same file,
// using the CompileOrc() and CompileSco() API calls is useful when
// the SCO or ORC are generated, or perhaps coming from another
// source, such as from a database or network.

package main

import "github.com/fggp/go-csnd6"

// Defining our Csound ORC code within a multiline String
var orc string = `
sr=48000
ksmps=32
nchnls=2
0dbfs=1

instr 1
aout vco2 0.5, 440
outs aout, aout
endin`

// Defining our Csound SCO code
var sco string = "i1 0 1"

func main() {
	c := csnd6.Create(nil)
	c.SetOption("-odac") // Using SetOption() to configure Csound
	// Note: use only one commandline flag at a time

	c.CompileOrc(orc) // Compile the Csound Orchestra string
	c.ReadScore(sco)  // Compile the Csound SCO String
	c.Start()         // When compiling from strings, this call is necessary before doing any performing
	c.Perform()       // Run Csound to completion
	c.Stop()
}
