// Example 7 - Communicating continuous values with Csound's Channel System
// Adapted for golang by Francois Pinot <fggpinot@gmail.com>, 2013.11.15,
// from the original python example by Steven Yi <stevenyi@gmail.com>
//
// This example introduces using Csound's Channel System to communicate
// continuous control data (k-rate) from a host program to Csound. The
// first thing to note is the RandomLine class. It takes in a base value
// and a range in which to vary randomly. The reset functions calculates
// a new random target value (self.end), a random duration in which to
// run (self.dur, expressed as // of audio blocks to last in duration), and
// calculates the increment value to apply to the current value per audio-block.
// When the target is met, the Randomline will reset itself to a new target
// value and duration.
//
// In this example, we use two RandomLine objects, one for amplitude and
// another for frequency. We start a Csound instrument instance that reads
// from two channels using the chnget opcode. In turn, we update the values
// to the channel from the host program. In this case, because we want to
// keep our values generating in sync with the audio engine, we use a
// while-loop instead of a CsoundPerformanceThread. To update the channel,
// we call the SetChannel method on the Csound object, passing a channel name
// and value. Note: The getValue method on the RandomLine not only gets
// us the current value, but also advances the internal state by the increment
// and by decrementing the duration.

package main

import (
	"github.com/fggp/go-csnd6"
	"math/rand"
)

type RandomLine struct {
	curVal, dur, increment, end float64
	base, lrange                float64
}

func NewRandomLine(base, lrange float64) *RandomLine {
	var rl RandomLine

	rl.base = base
	rl.lrange = lrange
	return &rl
}

// The receiver has to be a pointer because the Reset function
// changes the value of the receiver members
func (rl *RandomLine) Reset() {
	rl.dur = float64(256 + rand.Intn(256))
	rl.end = rand.Float64()
	rl.increment = (rl.end - rl.curVal) / rl.dur
}

// The receiver has to be a pointer because the Value function
// changes the value of the receiver members
func (rl *RandomLine) Value() csnd6.MYFLT {
	rl.dur -= 1
	if rl.dur < 0 {
		rl.Reset()
	}
	retVal := rl.curVal
	rl.curVal += rl.increment
	return csnd6.MYFLT(rl.base + rl.lrange*retVal)
}

// Our Orchestra for our project
var orc string = `
sr=48000
ksmps=32
nchnls=2
0dbfs=1

instr 1
kamp chnget "amp"
kfreq chnget "freq"
printk 0.5, kamp
printk 0.5, kfreq
aout vco2 kamp, kfreq
aout moogladder aout, 2000, 0.25
outs aout, aout
endin`

func main() {
	c := csnd6.Create(nil) // create an instance of Csound
	c.SetOption("-odac")   // Set option for Csound
	c.SetOption("-m7")     // Set option for Csound
	c.CompileOrc(orc)      // Compile Orchestra from String

	sco := "i1 0 60\n"

	c.ReadScore(sco) // Read in Score generated from notes

	c.Start() // When compiling from strings, this call is necessary before doing any performing

	amp := NewRandomLine(.4, .2)   // create RandomLine for use with Amplitude
	freq := NewRandomLine(400, 80) // create RandomLine for use with Frequency

	c.SetControlChannel("amp", amp.Value())   // Initialize channel value before running Csound
	c.SetControlChannel("freq", freq.Value()) // Initialize channel value before running Csound

	// The following is our main performance loop. We will perform one block of sound at a time
	// and continue to do so while it returns 0, which signifies to keep processing.

	for c.PerformKsmps() == 0 {
		c.SetControlChannel("amp", amp.Value())   // update channel value
		c.SetControlChannel("freq", freq.Value()) // update channel value
	}
	c.Stop()
}
