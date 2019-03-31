/* Example 7 - Communicating continuous values with Csound's Channel System
 * Adapted for Rust by Natanael Mojica <neithanmo@gmail.com>, 2019-01-28
 * from the original C example by Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example introduces using Csound's Channel System to communicate
 * continuous control data (k-rate) from a host program to Csound. The
 * first thing to note is random_line_create(). It takes in a base value
 * and a range in which to vary randomly.  The reset functions calculates
 * a new random target value (end), a random duration in which to
 * run (dur, expressed as # of audio blocks to last in duration), and
 * calculates the increment value to apply to the current value per audio-block.
 * When the target is met, the random_line_tick() function will call
 * random_line_reset() to update a new target value and duration.
 *
 * In this example, we use two random_line's, one for amplitude and
 * another for frequency.  We start a Csound instrument instance that reads
 * from two channels using the chnget opcode. In turn, we update the values
 * to the channel from the host program. To update the channel,
 * we call the csoundSetControlChannel function on the Csound struct, passing
 * a channel name and value.  Note: The random_line_tick() function not only
 * gets us the current value, but also advances the internal state by the
 * increment and by decrementing the duration.
 */
#![allow(non_camel_case_types, non_upper_case_globals, non_snake_case)]
extern crate csound;
use csound::*;

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
    rline.end = rand::random::<f64>(); // check
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

    /* Initialize channel values before running Csound */
    cs.set_control_channel("amp", random_line_tick(&mut amp));
    cs.set_control_channel("freq", random_line_tick(&mut freq));

    /* The following is our main performance loop. We will perform one
     * block of sound at a time and continue to do so while it returns false,
     * which signifies to keep processing.  We will explore this loop
     * technique in further examples.
     */
    while !cs.perform_ksmps() {
        /* Update Channel Values */
        cs.set_control_channel("amp", random_line_tick(&mut amp));
        cs.set_control_channel("freq", random_line_tick(&mut freq));
    }
    cs.stop();
}
