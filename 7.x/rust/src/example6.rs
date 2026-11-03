/* Example 6 - Generating a Score with a struct
 * Author: Steven Yi <stevenyi@gmail.com>
 * Updated for Csound 7 / csound-rs 0.3 by Natanael Mojica <neithanmo@gmail.com>
 *
 * This continues from example 5, rewriting it with a `Note` struct. It also
 * shows reusing an array of notes: the first pass plays them as-is, and the
 * second pass offsets the start times and transposes the pitch up a major
 * third.
 */

use csound::Csound;
use rand::Rng;
use std::fmt::Write;

#[derive(Default, Debug, Copy, Clone)]
struct Note {
    instr_id: u32,
    start: f64,
    duration: f64,
    amplitude: f64,
    midi_keynum: u32,
}

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

fn midi2pch(midi_keynum: u32) -> String {
    format!("{}.{:02}", (3 + (midi_keynum / 12)), (midi_keynum % 12))
}

fn generate_score() -> String {
    let mut notes = [Note::default(); 13];
    let mut rng = rand::rng();
    let mut retval = String::with_capacity(1024);

    /* Populate the notes */
    for (i, note) in notes.iter_mut().enumerate() {
        note.instr_id = 1;
        note.start = i as f64 * 0.25;
        note.duration = 0.25;
        note.amplitude = 0.5;
        note.midi_keynum = 60 + rng.random_range(0..15);
    }

    /* Convert the notes to a score string */
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

    /* Generate the notes again, transposed up a major third */
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
    println!("{retval}");
    retval
}

fn main() {
    let cs = Csound::new().expect("failed to create a Csound instance");

    /* Configure Csound (one command-line flag per call) */
    cs.set_option("-odac").unwrap();

    /* Compile the orchestra, then add the generated score before starting */
    cs.compile_orc(ORC, csound::Dispatch::Sync).unwrap();
    cs.send_string_event(&generate_score(), csound::Dispatch::Sync)
        .unwrap();

    /* Required before performing */
    cs.start().unwrap();

    /* Main performance loop */
    while !cs.perform_ksmps() { /* pass for now */ }
}
