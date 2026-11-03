/* Example 7 - Communicating continuous values with Csound's Channel System
 * Author: Steven Yi <stevenyi@gmail.com>
 * Adapted for Rust by Natanael Mojica <neithanmo@gmail.com>, updated for
 * Csound 7 / csound-rs 0.3.
 *
 * This introduces Csound's channel system for sending continuous (k-rate)
 * control data from the host into Csound. `random_line` produces a value that
 * drifts randomly towards a new target over a random number of audio blocks,
 * then picks a new target. We use two of them -- one for amplitude, one for
 * frequency -- and push their values into named channels each block with
 * `set_control_channel`. The instrument reads those channels with `chnget`.
 */

#![allow(non_camel_case_types, non_upper_case_globals, non_snake_case)]

use csound::Csound;

#[derive(Default)]
pub struct random_line {
    dur: i32,
    end: f64,
    increment: f64,
    current_val: f64,
    base: f64,
    range: f64,
}

/* Resets a random_line with new end, dur, and increment values */
fn random_line_reset(rline: &mut random_line) {
    rline.dur = (rand::random::<i32>() % 256) + 256;
    rline.end = rand::random::<f64>();
    rline.increment = (rline.end - rline.current_val) / (rline.dur as f64);
}

/* Creates a random_line and initializes values */
pub fn random_line_create(base: f64, range: f64) -> random_line {
    let mut retval = random_line {
        base,
        range,
        ..Default::default()
    };
    random_line_reset(&mut retval);
    retval
}

/* Advances the state of a random_line and returns the current value */
fn random_line_tick(rline: &mut random_line) -> f64 {
    let current_value = rline.current_val;
    rline.dur -= 1;
    if rline.dur <= 0 {
        random_line_reset(rline);
    }
    rline.current_val += rline.increment;
    rline.base + (current_value * rline.range)
}

/* Our Csound orchestra, as a multiline string */
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
    let cs = Csound::new().expect("failed to create a Csound instance");

    /* Configure Csound (one command-line flag per call) */
    cs.set_option("-odac").unwrap();

    /* Compile the orchestra, then add the score before starting */
    cs.compile_orc(ORC, csound::Dispatch::Sync).unwrap();
    cs.send_string_event("i1 0 60", csound::Dispatch::Sync)
        .unwrap();

    /* Required before performing */
    cs.start().unwrap();

    /* One random_line for amplitude, one for frequency */
    let mut amp = random_line_create(0.4, 0.2);
    let mut freq = random_line_create(400.0, 80.0);

    /* Initialize the channel values before running Csound */
    cs.set_control_channel("amp", random_line_tick(&mut amp))
        .unwrap();
    cs.set_control_channel("freq", random_line_tick(&mut freq))
        .unwrap();

    /* Main performance loop: update the channels each block */
    while !cs.perform_ksmps() {
        cs.set_control_channel("amp", random_line_tick(&mut amp))
            .unwrap();
        cs.set_control_channel("freq", random_line_tick(&mut freq))
            .unwrap();
    }
}
