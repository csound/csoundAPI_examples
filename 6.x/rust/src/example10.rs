/* Example 10 - More efficient Channel Communications
 * Adapted for Rust by Natanael Mojica <neithanmo@gmail.com>, 2019-01-30
 * from the original C example by Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example continues on from Example 9 and introduces a channel_updater
 * object. The create_channel_updater function will create and store a
 * MYFLT* that represents a Csound Channel. Additionally, it will store and
 * call a performance function pointer and store a void* for data to pass to
 * that performance function.  update_channel() will use the perf_func with the
 * data pointer to get a new value and set the value within the MYFLT* channel.
 *
 * This example continues the illustration of a progression of a project.  Note
 * that the process has changed a little bit where we now create a number of
 * ChannelUpdater objects and store them in an array.  The array is then
 * iterated through for updating the channel with the latest values.  In a
 * real-world project, this kind of scenario occurs when there are n-number of
 * items to update channels and one wants to have a flexible number that may
 * even change dynamically at runtime. For C, one would probably, use
 * a linked-list or other data structure than an array to represent a dynamic
 * list of channel_updaters. For this example, an array was deemed sufficient
 * to illustrate the concept.
 */

#![allow(non_camel_case_types, non_upper_case_globals, non_snake_case)]
extern crate csound;
use csound::{ControlChannelPtr, ControlChannelType, Csound};

extern crate rand;

/* Trait with update/rest functions*/
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
    /* Creates a RandomLine and initializes values */
    fn create(base: f64, range: f64) -> RandomLine {
        let mut retval = RandomLine::default();
        retval.base = base;
        retval.range = range;
        retval.reset();
        retval
    }
}

impl RandomFunc for RandomLine {
    /* Resets a RandomLine by calculating new end, dur, and increment values */
    fn reset(&mut self) {
        self.dur = (rand::random::<i32>() % 256) + 256;
        self.end = rand::random::<f64>();
        self.increment = (self.end - self.current_val) / (self.dur as f64);
    }

    /* Advances state of random line and returns current value */
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
    channel: ControlChannelPtr<'a>,
    data: T,
}

impl<'a, T: RandomFunc> Updater<'a, T> {
    fn create(csound: &'a Csound, channel_name: &str, data: T) -> Updater<'a, T> {
        let channel_pointer = create_channel(csound, channel_name);
        Updater {
            channel: channel_pointer,
            data,
        }
    }

    fn update(&mut self) {
        let mut value = [0.0; 1];
        value[0] = self.data.update();
        self.channel.write(&value);
    }
}

fn create_channel<'a>(csound: &'a Csound, channel_name: &str) -> ControlChannelPtr<'a> {
    match csound.get_channel_ptr(
        channel_name,
        ControlChannelType::CSOUND_CONTROL_CHANNEL | ControlChannelType::CSOUND_INPUT_CHANNEL,
    ) {
        Ok(ptr) => ptr,
        Err(status) => panic!("Channel not exists {:?}", status),
    }
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

    /* Create an array with two Channel Updaters */
    let mut updaters = [
        Updater::<RandomLine>::create(&cs, "amp", RandomLine::create(0.4, 0.2)),
        Updater::<RandomLine>::create(&cs, "freq", RandomLine::create(400.0, 80.0)),
    ];

    /* Initialize channel values before running Csound */
    for updater in updaters.iter_mut() {
        updater.update();
    }
    /* The following is our main performance loop. We will perform one
     * block of sound at a time and continue to do so while it returns false,
     * which signifies to keep processing.
     */
    while !cs.perform_ksmps() {
        /* Update Channel Values */
        for updater in updaters.iter_mut() {
            //update_channel(updater);
            updater.update();
        }
    }
    cs.stop();
}
