/* Example 10 - A channel updater abstraction
 * Author: Steven Yi <stevenyi@gmail.com>
 * Adapted for Rust by Natanael Mojica <neithanmo@gmail.com>, updated for
 * Csound 7 / csound-rs 0.3.
 *
 * This continues from example 9 and introduces an `Updater` that pairs a
 * channel with something that produces values for it (anything implementing
 * `RandomFunc`). We build several `Updater`s, store them in an array, and
 * iterate the array each block to push fresh values. In a real project this
 * is the shape you reach for when there is a dynamic number of things driving
 * channels.
 */

#![allow(non_camel_case_types, non_upper_case_globals, non_snake_case)]

use csound::{ControlChannel, Csound, InputChannel};

/* Trait with update/reset behaviour */
pub trait RandomFunc {
    fn reset(&mut self);
    fn update(&mut self) -> f64;
}

#[derive(Default)]
pub struct RandomLine {
    dur: i32,
    end: f64,
    increment: f64,
    current_val: f64,
    base: f64,
    range: f64,
}

impl RandomLine {
    fn create(base: f64, range: f64) -> RandomLine {
        let mut retval = RandomLine {
            base,
            range,
            ..Default::default()
        };
        retval.reset();
        retval
    }
}

impl RandomFunc for RandomLine {
    fn reset(&mut self) {
        self.dur = (rand::random::<i32>() % 256) + 256;
        self.end = rand::random::<f64>();
        self.increment = (self.end - self.current_val) / (self.dur as f64);
    }

    fn update(&mut self) -> f64 {
        let current_value = self.current_val;
        self.dur -= 1;
        if self.dur <= 0 {
            self.reset();
        }
        self.current_val += self.increment;
        self.base + (current_value * self.range)
    }
}

pub struct Updater<'a, T> {
    channel: InputChannel<'a, ControlChannel>,
    data: T,
}

impl<'a, T: RandomFunc> Updater<'a, T> {
    fn create(csound: &'a Csound, channel_name: &str, data: T) -> Updater<'a, T> {
        let channel = create_channel(csound, channel_name);
        Updater { channel, data }
    }

    fn update(&mut self) {
        let value = self.data.update();
        self.channel.lock().write(value);
    }
}

/* Fetch an input control channel, panicking with the reason if it fails */
fn create_channel<'a>(csound: &'a Csound, channel_name: &str) -> InputChannel<'a, ControlChannel> {
    csound
        .get_input_channel::<ControlChannel>(channel_name)
        .unwrap_or_else(|status| panic!("Channel does not exist {status:?}"))
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

    /* An array of channel updaters */
    let mut updaters = [
        Updater::create(&cs, "amp", RandomLine::create(0.4, 0.2)),
        Updater::create(&cs, "freq", RandomLine::create(400.0, 80.0)),
    ];

    /* Initialize the channel values before running Csound */
    for updater in updaters.iter_mut() {
        updater.update();
    }

    /* Main performance loop: update every channel each block */
    while !cs.perform_ksmps() {
        for updater in updaters.iter_mut() {
            updater.update();
        }
    }
}
