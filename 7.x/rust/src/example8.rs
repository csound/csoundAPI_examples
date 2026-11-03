/* Example 8 - More efficient Channel Communications
 * Author: Steven Yi <stevenyi@gmail.com>
 * Adapted for Rust by Natanael Mojica <neithanmo@gmail.com>, updated for
 * Csound 7 / csound-rs 0.3.
 *
 * This builds on example 7. `set_control_channel` is convenient but does a
 * channel lookup on every call. Here we fetch the channel once up front with
 * `get_input_channel::<ControlChannel>` and then write straight to it each
 * block. In csound-rs 0.3 the write goes through a short-lived lock guard
 * (`channel.lock().write(value)`), which keeps the access safe.
 */

#![allow(non_camel_case_types, non_upper_case_globals, non_snake_case)]

use csound::{ControlChannel, Csound};

#[derive(Default)]
pub struct random_line {
    dur: i32,
    end: f64,
    increment: f64,
    current_val: f64,
    base: f64,
    range: f64,
}

fn random_line_reset(rline: &mut random_line) {
    rline.dur = (rand::random::<i32>() % 256) + 256;
    rline.end = rand::random::<f64>();
    rline.increment = (rline.end - rline.current_val) / (rline.dur as f64);
}

pub fn random_line_create(base: f64, range: f64) -> random_line {
    let mut retval = random_line {
        base,
        range,
        ..Default::default()
    };
    random_line_reset(&mut retval);
    retval
}

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

    cs.set_option("-odac").unwrap();

    cs.compile_orc(ORC, csound::Dispatch::Sync).unwrap();
    cs.send_string_event("i1 0 60", csound::Dispatch::Sync)
        .unwrap();
    cs.start().unwrap();

    let mut amp = random_line_create(0.4, 0.2);
    let mut freq = random_line_create(400.0, 80.0);

    /* Fetch the input control channels once, up front */
    let amp_channel = cs
        .get_input_channel::<ControlChannel>("amp")
        .expect("amp channel");
    let freq_channel = cs
        .get_input_channel::<ControlChannel>("freq")
        .expect("freq channel");

    /* Initialize the channel values before running Csound */
    amp_channel.lock().write(random_line_tick(&mut amp));
    freq_channel.lock().write(random_line_tick(&mut freq));

    /* Main performance loop: write directly to the channels each block */
    while !cs.perform_ksmps() {
        amp_channel.lock().write(random_line_tick(&mut amp));
        freq_channel.lock().write(random_line_tick(&mut freq));
    }
}
