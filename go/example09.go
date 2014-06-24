// Example 9 - More efficient Channel Communications
// Adapted for golang by Francois Pinot <fggpinot@gmail.com>, 2013.11.15,
// from the original python example by Steven Yi <stevenyi@gmail.com>
//
//
// This example continues on from Example 8 and just refactors the
// creation and setup of []csnd6.MYFLT's into a createChannel()
// function. This example illustrates some natural progression that
// might occur in your own API-based projects, and how you might
// simplify your own code.

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

func createChannel(csound csnd6.CSOUND, channelName string) []csnd6.MYFLT {
	chn, err := csound.ChannelPtr(channelName,
		csnd6.CSOUND_CONTROL_CHANNEL|csnd6.CSOUND_INPUT_CHANNEL)
	if err != nil {
		panic(err)
	}
	return chn
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

	ampChannel := createChannel(c, "amp") // uses utility method to create a channel and get a []csnd6.MYFLT
	freqChannel := createChannel(c, "freq")

	amp := NewRandomLine(.4, .2)
	freq := NewRandomLine(400, 80)

	ampChannel[0] = amp.Value()
	freqChannel[0] = freq.Value()

	for c.PerformKsmps() == 0 {
		ampChannel[0] = amp.Value()
		freqChannel[0] = freq.Value()
	}
	c.Stop()
}
