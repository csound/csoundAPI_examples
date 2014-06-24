// Example 8 - More efficient Channel Communications
// Adapted for golang by Francois Pinot <fggpinot@gmail.com>, 2013.11.15,
// from the original python example by Steven Yi <stevenyi@gmail.com>
//
// This example builds on Example 7 by replacing the calls to SetChannel
// with using ChannelPtr. In the Csound API, using SetChannel and Channel
// is great for quick work, but ultimately it is slower than pre-fetching the
// actual channel pointer. This is because SetChannel/Channel operates by doing a
// lookup of the Channel Pointer, then setting or getting the value. This
// happens on each call. The alternative is to use ChannelPtr, which fetches
// the Channel Pointer and lets you directly set and get the value on the pointer.
//
// In C/C++/Objective-C, one can directly use MYFLT* to get/set values. However,
// for Google Go language it is not possible to get/set the value on the pointer
// itself. The Csound API for Goggle Go language uses a special wrapper object
// called a slice of csnd6.MYFLT, which will hold a reference to a MYFLT*.
// The []csnd6.MYFLT in turn is used in the classical Go way for setting
// and getting values.
//
// The code below shows how to use the []csnd6.MYFLT in conjunction with ChannelPtr
// to have a more optimized channel setting system.

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

	// the following calls store the Channel Pointer retreived from Csound into the
	// returned []csnd6.MYFLT slices
	ampChannel, err := c.ChannelPtr("amp",
		csnd6.CSOUND_CONTROL_CHANNEL|csnd6.CSOUND_INPUT_CHANNEL)
	if err != nil {
		panic(err)
	}
	freqChannel, err := c.ChannelPtr("freq",
		csnd6.CSOUND_CONTROL_CHANNEL|csnd6.CSOUND_INPUT_CHANNEL)
	if err != nil {
		panic(err)
	}

	amp := NewRandomLine(.4, .2)
	freq := NewRandomLine(400, 80)

	ampChannel[0] = amp.Value() // note we are now setting values on the []csnd6.MYFLT slice
	freqChannel[0] = freq.Value()

	for c.PerformKsmps() == 0 {
		ampChannel[0] = amp.Value()
		freqChannel[0] = freq.Value()
	}
	c.Stop()
}
