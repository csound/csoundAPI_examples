/* Example 5 - Generating a Score
 * Author: Steven Yi <stevenyi@gmail.com>
 * Updated for Csound 7 / csound-rs 0.3 by Natanael Mojica <neithanmo@gmail.com>
 *
 * This example looks at generating the score instead of writing it by hand.
 * `generate_line` builds an ascending note line with a format string;
 * `generate_random` fills a 2-D array of note data and then formats it into a
 * score string. The second pattern is common when an event-based model of
 * music is backed by a data structure (an array here, but it could be structs).
 *
 * Swap which `send_string_event` call is active below to hear each generator.
 */

use csound::Csound;
use rand::Rng;
use std::fmt::Write;
use std::thread;

/* Our Csound orchestra, as a multiline string */
static ORC: &str = "sr=44100
  ksmps=32
  nchnls=2
  0dbfs=1
  instr 1
  ipch = cps2pch(p5, 12)
  kenv linsegr 0, .05, 1, .05, .7, .4, 0
  aout vco2 p4 * kenv, ipch
  aout moogladder aout, 2000, 0.25
  outs aout, aout
endin";

/* A simple, hand-written static score */
#[allow(dead_code)]
static SCO: &str = "i1 0 1 0.5 8.00";

/* Generate an ascending note line */
#[allow(dead_code)]
fn generate_line() -> String {
    let mut retval = String::with_capacity(1024);
    for i in 0..13 {
        writeln!(&mut retval, "i1 {} .25 .5 8.{:02}", (i as f64) * 0.25, i).unwrap();
    }
    println!("{retval}");
    retval
}

/* Generate a sequence of notes with random pitches via a 2-D array */
// Index-based access to the parallel score columns mirrors the original C and
// Python examples and reads more clearly than an iterator chain here.
#[allow(clippy::needless_range_loop)]
fn generate_random() -> String {
    let mut rng = rand::rng();

    let mut retval = String::with_capacity(1024);
    let mut values = [[0f64; 13]; 5];

    /* Populate the array */
    for i in 0..13 {
        values[0][i] = 1f64;
        values[1][i] = i as f64 * 0.25;
        values[2][i] = 0.25;
        values[3][i] = 0.5;
        values[4][i] = rng.random_range(0.0..15.0);
    }

    /* Convert the array to a score string */
    for i in 0..13 {
        writeln!(
            &mut retval,
            "i{} {} {}  {} 8.{:02}",
            values[0][i] as u32, values[1][i], values[2][i], values[3][i], values[4][i] as u32
        )
        .unwrap();
    }
    println!("{retval}");
    retval
}

fn main() {
    let cs = Csound::new().expect("failed to create a Csound instance");

    /* Configure Csound (one command-line flag per call) */
    cs.set_option("-odac").unwrap();

    /* Compile the orchestra string */
    cs.compile_orc(ORC, csound::Dispatch::Sync).unwrap();

    /* Send the generated score before starting. Try the other generators:
     * cs.send_string_event(SCO, csound::Dispatch::Sync).unwrap();
     * cs.send_string_event(&generate_line(), csound::Dispatch::Sync).unwrap(); */
    cs.send_string_event(&generate_random(), csound::Dispatch::Sync)
        .unwrap();

    /* Required before performing */
    cs.start().unwrap();

    /* Perform on a separate thread, as in example 4 */
    let child = thread::spawn(move || while !cs.perform_ksmps() {});

    child.join().unwrap();
}
