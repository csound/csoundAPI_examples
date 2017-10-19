// Example 5 - Generating Score
// Adapted for golang by Francois Pinot <fggpinot@gmail.com>, 2013.11.15,
// from the original python example by Steven Yi <stevenyi@gmail.com>
//
// In this example, we will look at three techniques for generating our Score.
//
// The first is one we have already seen, which is to just write out the score
// by hand as a String.
//
// Knowing that we pass strings into Csound to pass note events, we can also
// generate the string. In the second example, sco2 starts as an empty string.
// Using a for-loop, we append to sco2 note strings using a string formatting
// string that has its replacement values replaced. The replace values are
// calculated using the i value, and the result is an ascending note line.
//
// In the final example, we are going to generate a list of lists. The top-level
// list represents our score as a whole, and each sub-list within it represents
// the data for a single note. The main list is then processed in two ways: first,
// it processes each sub-list and joins the values together into a single note string;
// second, it joins each individual note string into a single, large score string,
// separated by newlines. The end result is a sequence of 13 notes with random
// pitches.
//
// The final example represents a common pattern of development. For systems that
// employ some event-based model of music, it is common to use some kind of data
// structure to represent events. This may use some kind of common data structure
// like a list, or it may be represented by using a class and instances of that
// class.
//
// Note, the three examples here are indicated with comments. To listen to the examples,
// look for the lines that have c.ReadScore(sco) (lines 80-82), uncomment the one
// you want to hear, and comment out the others.

package main

import (
	"fmt"
	"github.com/fggp/go-csnd6"
	"math/rand"
	"strconv"
	"strings"
)

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

// Example 1 - Static Score
var sco string = "i1 0 1 0.5 8.00"
var sco2, sco3 string

func main() {
	c := csnd6.Create(nil) // create an instance of Csound
	c.SetOption("-odac")   // Set option for Csound
	c.CompileOrc(orc)      // Compile Orchestra from String

	// Example 2 - Generating Score string with a loop
	for i := 0; i < 13; i++ {
		sco2 += fmt.Sprintf("i1 %g .25 0.5 8.%02g\n", float64(i)*0.25, float64(i))
	}
	//	fmt.Println(sco2)

	// Example 3 - Generating Score using intermediate data structure (list of lists),
	//             then converting to String.
	var valsf [][]float64     // initialize a list to hold lists of values
	for i := 0; i < 13; i++ { // populate that list
		pitch, _ := strconv.ParseFloat(fmt.Sprintf("8.%02g", float64(rand.Intn(15))), 64)
		valsf = append(valsf, []float64{1, float64(i) * 0.25, 0.25, 0.5, pitch})
	}
	var vals []string
	for _, a := range valsf { // convert list of lists into a list of strings
		s := fmt.Sprint(a)
		vals = append(vals, "i"+(s[1:len(s)-1]))
	}

	// now convert that list of strings into a single string
	sco3 = strings.Join(vals, "\n")
	//	fmt.Println(vals)
	//	fmt.Println(sco3)

	c.ReadScore(sco) // Read in Score from pre-written String
	//c.ReadScore(sco2) // Read in Score from loop-generated String
	//c.ReadScore(sco3) // Read in Score from loop-generated String

	c.Start() // When compiling from strings, this call is necessary before doing any performing

	// The following is our main performance loop. We will perform one block of sound at a time
	// and continue to do so while it returns 0, which signifies to keep processing.
	for c.PerformKsmps() == 0 {
	}
	c.Stop()
}
