// Example 13 - CsoundPerformanceThread process callback.
// Author Francois Pinot <fggpinot@gmail.com>, 2013.11.27,
//
// This example is a mix of examples 4 and 10. We use a
// CsoundPerformanceThread and we do regular processing (channel updates)
// through a callback function passed to the thread. This callback function
// is invoked by the thread for each ksmps block.

package main

import (
	"github.com/fggp/go-csnd6"
	"github.com/fggp/go-csperfthread"
	"math/rand"
	"unsafe"
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

type Updater interface {
	Value() csnd6.MYFLT
}

func createChannel(csound csnd6.CSOUND, channelName string) []csnd6.MYFLT {
	chn, err := csound.ChannelPtr(channelName,
		csnd6.CSOUND_CONTROL_CHANNEL|csnd6.CSOUND_INPUT_CHANNEL)
	if err != nil {
		panic(err)
	}
	return chn
}

type ChannelUpdater struct {
	updater Updater
	channel []csnd6.MYFLT
}

func NewChannelUpdater(csound csnd6.CSOUND, channelName string, updater Updater) ChannelUpdater {
	var cu ChannelUpdater

	cu.updater = updater
	cu.channel = createChannel(csound, channelName)
	return cu
}

func (cu ChannelUpdater) Update() {
	cu.channel[0] = cu.updater.Value()
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
kres chnget "resonance"
printk 0.5, kamp
printk 0.5, kfreq
printk 0.5, kres
aout vco2 kamp, kfreq
aout moogladder aout, 2000, kres
outs aout, aout
endin`

// The process callback function. It will receive an unsafe.Pointer type
// corresponding to the C void* type. It is the responsibility of the user
// to know the underlying type that is pointed to, so that the correct cast
// is done before using the data.
func processCallback(data unsafe.Pointer) {
	channels := (*[]ChannelUpdater)(data)
	for _, chn := range *channels {
		chn.Update()
	}
}

func main() {
	c := csnd6.Create(nil) // create an instance of Csound
	c.SetOption("-odac")   // Set option for Csound
	c.SetOption("-m7")     // Set option for Csound
	c.CompileOrc(orc)      // Compile Orchestra from String

	sco := "i1 0 60\n"

	c.ReadScore(sco) // Read in Score generated from notes

	c.Start() // When compiling from strings, this call is necessary before doing any performing

	// Create a set of ChannelUpdaters
	channels := []ChannelUpdater{
		NewChannelUpdater(c, "amp", NewRandomLine(.4, .2)),
		NewChannelUpdater(c, "freq", NewRandomLine(400, 80)),
		NewChannelUpdater(c, "resonance", NewRandomLine(.4, .3)),
	}

	// Initialize all ChannelUpdaters
	for _, chn := range channels {
		chn.Update()
	}

	t := csperfthread.NewCsoundPerformanceThread(c) // Create a new CsoundPerformanceThread, passing in the Csound object

	// We set the thread process callback function and we transmit to the thread
	// an untyped pointer to some user data. This pointer will be passed to the callback
	// each time it is invoked by the thread.
	t.SetProcessCallback(processCallback, unsafe.Pointer(&channels))

	t.Play() // starts the thread, which is now running separately from the main thread. This
	// call is asynchronous and will immediately return back here to continue code
	// execution.
	t.Join() // Join will wait for the other thread to complete. If we did not call Join(),
	// after t.Play() returns we would immediate move to the next line, c.Stop().
	// That would stop Csound without really giving it time to run.

	c.Stop()    // stops Csound
	c.Cleanup() // clean up Csound; this is useful if you're going to reuse a Csound instance
}
