/* Example 5 - Generating Score
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * In this example, we will look at three techniques for generating our Score.
 *
 * The first is one we have already seen, which is to just write out the score
 * by hand as a String.
 *
 * Knowing that we pass strings into Csound to pass note events, we can also
 * generate the string.  In the second example, generate_sco2 starts with an empty string.
 * Using a for-loop, we append to retval note strings using a string formatting
 * string that has its replacement values replaced.  The replace values are
 * calculated using the i value, and the result is an ascending note line.
 *
 * In the final example, we are going to generate a 2-dimensional array.  The top-level
 * array represents our score as a whole, and each sub-array within it represents
 * the data for a single note.  The values are populated and then a second step is done
 * to format the values into a string and to concatenate that value to the retval string.
 * The end result is a sequence of 13 notes with random pitches.
 *
 * The final example represents a common pattern of development.  For systems that
 * employ some event-based model of music, it is common to use some kind of data
 * structure to represent events.  This may use some kind of common data structure
 * like an array, or it may be represented by using a struct and instances of that
 * struct.
 *
 * Note, the three examples here are indicated with comments.  To listen to the examples,
 * look for the lines that have csoundReadScore() (lines 117-123), uncomment the one
 * you want to hear, and comment out the others.
 */

extern crate csound;
use csound::*;
use std::fmt::Write;

extern crate rand;
use rand::Rng;

use std::sync::{Arc, Mutex};
use std::thread;

/* Defining our Csound ORC code within a multiline String */
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

/* Example 1 - Static Score */
static SCO: &str = "i1 0 1 0.5 8.00";

fn generate_example2() -> String {
    let mut retval = String::with_capacity(1024);
    for i in 0..13 {
        writeln!(&mut retval, "i1 {} .25 .5 8.{:02}", (i as f64) * 0.25, i).unwrap();
    }
    println!("{}", retval);
    retval
}

fn generate_example3() -> String {
    let mut rng = rand::thread_rng();

    let mut retval = String::with_capacity(1024);
    let mut values = [[0f64; 13]; 5];

    /* Populate array */
    for i in 0..13 {
        values[0][i] = 1f64;
        values[1][i] = i as f64 * 0.25;
        values[2][i] = 0.25;
        values[3][i] = 0.5;
        values[4][i] = rng.gen_range(0.0, 15.0);
    }

    /* Convert array to to String */
    for i in 0..13 {
        writeln!(
            &mut retval,
            "i{} {} {}  {} 8.{:02}",
            values[0][i] as u32, values[1][i], values[2][i], values[3][i], values[4][i] as u32
        )
        .unwrap();
    }
    println!("{}", retval);
    retval
}

fn main() {
    let mut cs = Csound::new();

    /* Using SetOption() to configure Csound
    Note: use only one commandline flag at a time */
    cs.set_option("-odac").unwrap();

    /* Compile the Csound Orchestra string */
    cs.compile_orc(ORC).unwrap();

    /* Compile the Csound SCO String */
    //cs.read_score(&generate_example3()).unwrap();
    cs.read_score(&generate_example3()).unwrap();

    /* When compiling from strings, this call is necessary
     * before doing any performing */
    cs.start().unwrap();

    /* Create a new thread that will use our performance function and
     * pass in our CSOUND structure. This call is asynchronous and
     * will immediately return back here to continue code execution
     */
    let cs = Arc::new(Mutex::new(cs));
    let cs = Arc::clone(&cs);

    let child = thread::spawn(move || {
        while !cs.lock().unwrap().perform_ksmps() { /* pass for now */ }
    });

    child.join().unwrap();
}
