/* Example 8 - More efficient Channel Communications
 * Adapted for Rust by Natanael Mojica <neithanmo@gmail.com>, 2019-01-28
 * from the original C example by Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example builds on Example 7 by replacing the calls to SetControlChannel
 * with using csoundGetChannelPtr. In the Csound API, using SetControlChannel
 * and GetControlChannel is great for quick work, but ultimately it is slower
 * than pre-fetching the actual channel pointer.  This is because
 * Set/GetControlChannel operates by doing a lookup of the Channel Pointer,
 * then setting or getting the value.  This happens on each call. The
 * alternative is to use csoundGetChannelPtr, which fetches the Channel Pointer
 * and lets you directly set and get the value on the pointer.
 *
 * One thing to note though is that csoundSetControlChannel is protected by
 * spinlocks.  This means that it is safe for multithreading to use.  However,
 * if you are working with your own performance-loop, you can correctly process
 * updates to channels and there will be no problems with multithreading.
 *
 */

#![allow(non_camel_case_types, non_upper_case_globals, non_snake_case)]
extern crate csound;
use csound::{ControlChannelType, Csound};

extern crate rand;

#[derive(Default)]
pub struct random_line {
    dur: i32,
    end: f64,
    increment: f64,
    current_val: f64,
    base: f64,
    range: f64,
}

/* Resets a random_line by calculating new end, dur, and increment values */
fn random_line_reset(rline: &mut random_line) {
    rline.dur = (rand::random::<i32>() % 256) + 256;
    rline.end = rand::random::<f64>();
    rline.increment = (rline.end - rline.current_val) / (rline.dur as f64);
}

/* Creates a random_line and initializes values */
pub fn random_line_create(base: f64, range: f64) -> random_line {
    let mut retval = random_line::default();
    retval.base = base;
    retval.range = range;
    random_line_reset(&mut retval);
    retval
}

/* Advances state of random line and returns current value */
fn random_line_tick(rline: &mut random_line) -> f64 {
    let current_value = rline.current_val;
    rline.dur -= 1;
    if rline.dur <= 0 {
        random_line_reset(rline);
    }
    rline.current_val += rline.increment;
    rline.base + (current_value * rline.range)
}

/* Defining our Csound ORC code within a multiline String */
static ORC: &str = "sr=44100
  ksmps=32
  nchnls=2
  0dbfs=1
  instr 1
  kamp chnget \"amp\"
  kfreq chnget \"freq\"
  printk 0.5, kamp
  printk 0.5, kfreq
  aout vco2 kamp, kfreq
  aout moogladder aout, 2000, 0.25
  outs aout, aout
endin";

fn main() {
    let mut cs = Csound::new();

    /* Using SetOption() to configure Csound
    Note: use only one commandline flag at a time */
    cs.set_option("-odac").unwrap();

    /* Compile the Csound Orchestra string */
    cs.compile_orc(ORC).unwrap();

    /* Compile the Csound SCO String */
    cs.read_score("i1 0 60").unwrap();

    /* When compiling from strings, this call is necessary
     * before doing any performing */
    cs.start().unwrap();

    /* Create a random_line for use with Amplitude */
    let mut amp = random_line_create(0.4, 0.2);

    /* Create a random_line for use with Frequency */
    let mut freq = random_line_create(400.0, 80.0);

    /* Retrieve Channel Pointers from Csound */
    let amp_channel = cs
        .get_channel_ptr(
            "amp",
            ControlChannelType::CSOUND_CONTROL_CHANNEL | ControlChannelType::CSOUND_INPUT_CHANNEL,
        )
        .unwrap();
    let freq_channel = cs
        .get_channel_ptr(
            "freq",
            ControlChannelType::CSOUND_CONTROL_CHANNEL | ControlChannelType::CSOUND_INPUT_CHANNEL,
        )
        .unwrap();

    /* Initialize channel values before running Csound */
    let mut amp_value = [random_line_tick(&mut amp); 1];
    let mut freq_value = [random_line_tick(&mut freq); 1];

    /* The following is our main performance loop. We will perform one
     * block of sound at a time and continue to do so while it returns false,
     * which signifies to keep processing.  We will explore this loop
     * technique in further examples.
     */
    while !cs.perform_ksmps() {
        /* Update Channel Values */
        amp_value[0] = random_line_tick(&mut amp);
        freq_value[0] = random_line_tick(&mut freq);
        amp_channel.write(&amp_value);
        freq_channel.write(&freq_value);
    }
    cs.stop();
}
