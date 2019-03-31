/* Example 6 - Generating Score
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example continues on from Example 5, rewriting the example using
 * a struct called Note. This example also shows how an array of notes
 * could be used multiple times.  The first loop through we use the notes
 * as-is, and during the second time we generate the notes again with
 * the same properties except we alter the fifth p-field up 4 semitones
 * and offset the start time.
 */

extern crate csound;
use csound::*;
use std::fmt::Write;

extern crate rand;
use rand::Rng;

#[derive(Default, Debug, Copy, Clone)]
struct Note {
    instr_id: u32,
    start: f64,
    duration: f64,
    amplitude: f64,
    midi_keynum: u32,
}

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

fn midi2pch(midi_keynum: u32) -> String {
    format!("{}.{:02}", (3 + (midi_keynum / 12)), (midi_keynum % 12))
}

fn generate_score() -> String {
    let mut notes = [Note::default(); 13];
    let mut rng = rand::thread_rng();
    let mut retval = String::with_capacity(1024);

    /* Populate Notes */
    for (i, note) in notes.iter_mut().enumerate() {
        note.instr_id = 1;
        note.start = i as f64 * 0.25;
        note.duration = 0.25;
        note.amplitude = 0.5;
        note.midi_keynum = 60 + rng.gen_range(0, 15);
    }

    /* Convert notes to to String */
    for note in &notes {
        writeln!(
            &mut retval,
            "i{} {} {}  {} {}",
            note.instr_id,
            note.start,
            note.duration,
            note.amplitude,
            midi2pch(note.midi_keynum)
        )
        .unwrap();
    }

    /* Generate notes again transposed a Major 3rd up */
    for note in &notes {
        writeln!(
            &mut retval,
            "i{} {} {}  {} {}",
            note.instr_id,
            note.start + 0.125,
            note.duration,
            note.amplitude,
            midi2pch(note.midi_keynum + 4)
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
    cs.read_score(&generate_score()).unwrap();

    /* When compiling from strings, this call is necessary
     * before doing any performing */
    cs.start().unwrap();

    /* The following is our main performance loop. We will perform one
     * block of sound at a time and continue to do so while it returns false,
     * which signifies to keep processing.  We will explore this loop
     * technique in further examples.
     */
    while !cs.perform_ksmps() { /* pass for now */ }
    cs.stop();
}
