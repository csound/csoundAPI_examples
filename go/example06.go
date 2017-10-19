// Example 6 - Generating Score
// Adapted for golang by Francois Pinot <fggpinot@gmail.com>, 2013.11.15,
// from the original python example by Steven Yi <stevenyi@gmail.com>
//
// This example continues on from Example 5, rewriting the example using
// a Class called Note. The note example has its __str__ method implemented
// to generate a well-formatted Csound SCO note.
//
// This example also shows how a list of notes could be used multiple times.
// The first loop through we use the notes as-is, and during the second time
// we generate the notes again with the same properties except we alter the
// fifth p-field up 4 semitones.
//
// Note: Altering a Notes values like this is alright for this example, but
// it is a destructive edit. Real world code might make copies of Notes or
// alter the score generation to maintain the original values.

package main

import (
	"fmt"
	"github.com/fggp/go-csnd6"
	"math/rand"
)

func midi2pch(num int) string {
	return fmt.Sprintf("%d.%02g", 3+num/12, float64(num%12))
}

type Note []float64

func NewNote(args ...float64) Note {
	return Note(args)
}

func (n Note) String() string {
	retVal := "i"
	for i, p := range n {
		if i == 4 {
			retVal += " " + midi2pch(int(p))
		} else {
			retVal += " " + fmt.Sprint(p)
		}
	}
	return retVal
}

// Our Orchestra for our project
var orc string = `
sr=48000
ksmps=32
nchnls=2
0dbfs=1

instr 1
ipch = cps2pch(p5, 12)
kenv linsegr 0, .05, 1, .05, .7, .4, 0
aout vco2 p4 * kenv, ipch
aout moogladder aout, 2000, 0.25
outs aout, aout
endin`

func main() {
	c := csnd6.Create(nil) // create an instance of Csound
	c.SetOption("-odac")   // Set option for Csound
	c.CompileOrc(orc)      // Compile Orchestra from String

	var notes []Note          // initialize a list to hold lists of values
	for i := 0; i < 13; i++ { // populate that list
		notes = append(notes, NewNote(1, float64(i)*0.25, .25, 0.5,
			float64(60+rand.Intn(15))))
	}

	// now convert list of Note objects to string
	var sco string
	for _, n := range notes {
		sco += fmt.Sprintf("%s\n", n) // this implicitly calls the String method on the Note type
	}

	// generate notes again transposed a Major 3rd up
	for _, n := range notes {
		n[4] += 4
		n[1] += .125
		sco += fmt.Sprintf("%s\n", n)
	}

	fmt.Println(sco)

	c.ReadScore(sco) // Read in Score generated from notes

	c.Start() // When compiling from strings, this call is necessary before doing any performing

	// The following is our main performance loop. We will perform one block of sound at a time
	// and continue to do so while it returns 0, which signifies to keep processing.

	for c.PerformKsmps() == 0 {
	}

	c.Stop()
}
