/* Example 4 - Using Threads
 * Author: Steven Yi <stevenyi@gmail.com>
 * Updated for Csound 7 / csound-rs 0.3 by Natanael Mojica <neithanmo@gmail.com>
 *
 * Here we run Csound in a separate thread. This is a common scenario: Csound
 * performs in one thread while the main thread does other work (a GUI, a
 * worker for heavy computation, etc.).
 *
 * The original C/Python examples used Csound's own thread helpers
 * (`CsoundPerformanceThread`). The csound-rs 0.3 bindings do not wrap that
 * C++ helper, and we don't need it: `Csound` is `Send`, so we wrap it in an
 * `Arc<Mutex<..>>` and run the `perform_ksmps` loop on a std thread.
 */

use csound::Csound;
use std::thread;

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
static SCO: &str = "i1 0 10";

fn main() {
    let cs = Csound::new().expect("failed to create a Csound instance");

    /* Configure Csound (one command-line flag per call) */
    cs.set_option("-odac").unwrap();

    /* Compile the orchestra, then add the score before starting */
    cs.compile_orc(ORC, csound::Dispatch::Sync).unwrap();
    cs.send_string_event(SCO, csound::Dispatch::Sync).unwrap();

    /* Required before performing */
    cs.start().unwrap();

    /* Move Csound into a worker thread and run the performance loop there,
     * leaving the main thread free to do other work while audio renders.
     * `Csound` is `Send`, so a plain move is enough. If the main thread also
     * needed to touch Csound, you'd share it with an `Arc<Mutex<Csound>>`. */
    let child = thread::spawn(move || while !cs.perform_ksmps() {});

    child.join().unwrap();
}
