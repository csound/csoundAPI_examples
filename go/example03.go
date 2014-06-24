// Example 3 - Using our own performance loop
// Adapted for golang by Francois Pinot <fggpinot@gmail.com>, 2013.11.15,
// from the original python example by Steven Yi <stevenyi@gmail.com>
//
// In this example, we use a while loop to perform Csound one audio block at a time.
// This technique is important to know as it will allow us to do further processing
// safely at block boundaries. We will explore the technique further in later examples.

package main

import "github.com/fggp/go-csnd6"

// Our Orchestra for our project
var orc string = `
sr=48000
ksmps=32
nchnls=2
0dbfs=1

instr 1
aout vco2 0.5, 440
outs aout, aout
endin`

// Our Score for our project
var sco string = "i1 0 1"

func main() {
	c := csnd6.Create(nil) // create an instance of Csound
	c.SetOption("-odac")   // Set option for Csound
	c.CompileOrc(orc)      // Compile Orchestra from String
	c.ReadScore(sco)       // Read in Score from String
	c.Start()              // When compiling from strings, this call is necessary before doing any performing

	// The following is our main performance loop. We will perform one block of sound at a time
	// and continue to do so while it returns 0, which signifies to keep processing.  We will
	// explore this loop technique in further examples.
	for c.PerformKsmps() == 0 {
	}

	c.Stop()
}
