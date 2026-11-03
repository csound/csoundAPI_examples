/* Example 2 - Compilation with Csound without a CSD
 * Author: Steven Yi <stevenyi@gmail.com>
 * Updated for Csound 7 / csound-rs 0.3 by Natanael Mojica <neithanmo@gmail.com>
 *
 * Here we move from an external CSD file to embedding the orchestra and
 * score directly in our program. This is useful when the orchestra or score
 * is generated at runtime, or comes from another source such as a database
 * or the network.
 *
 * Csound 7 note: `csoundReadScore` is gone. Score statements are now sent as
 * events with `send_string_event` (the wrapper over `csoundEventString`).
 * Send them *before* `start()`: the score is then known up front, so the
 * `perform_ksmps` loop ends when the score ends. (Sending events after
 * `start()` puts Csound in real-time mode, where performance runs forever.)
 * We drive performance with our own `perform_ksmps` loop, since `csoundPerform`
 * was also removed in Csound 7.
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

    /* Compile the orchestra string */
    cs.compile_orc(ORC, csound::Dispatch::Sync).unwrap();

    /* Send the score before starting, so performance ends with the score */
    cs.send_string_event(SCO, csound::Dispatch::Sync).unwrap();

    /* Starting the engine is required before performing */
    cs.start().unwrap();

    /* Perform one block at a time until the score ends */
    while !cs.perform_ksmps() {}
}
