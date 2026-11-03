/* Example 3 - Using our own performance loop
 * Author: Steven Yi <stevenyi@gmail.com>
 * Updated for Csound 7 / csound-rs 0.3 by Natanael Mojica <neithanmo@gmail.com>
 *
 * Here we drive Csound one audio block at a time with a `while` loop. This is
 * important to know, as it lets us do further processing safely at block
 * boundaries, which later examples build on.
 */

use csound::Csound;

/* Our Csound orchestra, as a multiline string */
static ORC: &str = "sr=44100
  ksmps=32
  nchnls=2
  0dbfs=1
  instr 1
  aout vco2 0.5, 440
  outs aout, aout
endin";

/* Our Csound score */
static SCO: &str = "i1 0 1";

fn main() {
    let cs = Csound::new().expect("failed to create a Csound instance");

    /* Configure Csound (one command-line flag per call) */
    cs.set_option("-odac").unwrap();

    /* Compile the orchestra, then add the score before starting */
    cs.compile_orc(ORC, csound::Dispatch::Sync).unwrap();
    cs.send_string_event(SCO, csound::Dispatch::Sync).unwrap();

    /* Required before performing */
    cs.start().unwrap();

    /* Our main performance loop: perform one block of sound at a time and keep
     * going while `perform_ksmps` returns false (true means the score ended). */
    while !cs.perform_ksmps() { /* pass for now */ }
}
