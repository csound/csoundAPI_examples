/* Example 4 - Using Threads
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * In this example, we use the Csound thread functions to run Csound in
 * a separate thread. This is a common scenario where you will run
 * Csound in one thread, and doing other things in another thread
 * (i.e. have a GUI main thread, maybe a worker thread for heavy
 * computations, etc.).
 *
 * The Python example used the CsoundPerformanceThread which is a
 * C++ class that uses the same C functions used in this example.
 * To note, Csound offers thread functions so that the the developer
 * won't have to worry about what thread library is used (i.e. pthreads).
 * Using Csound's thread functions helps make your code more portable
 * between platforms.
 */

extern crate csound;
use csound::*;

use std::sync::{Arc, Mutex};
use std::thread;

/* Defining our Csound ORC code within a multiline String */
static orc: &str = "sr=44100
  ksmps=32
  nchnls=2
  0dbfs=1
  instr 1
  aout vco2 0.5, 440
  outs aout, aout
endin";

/*Defining our Csound SCO code */
static sco: &str = "i1 0 10";

fn main() {
    let mut cs = Csound::new();

    /* Using SetOption() to configure Csound
    Note: use only one commandline flag at a time */
    cs.set_option("-odac");

    /* Compile the Csound Orchestra string */
    cs.compile_orc(orc).unwrap();

    /* Compile the Csound SCO String */
    cs.read_score(sco).unwrap();

    /* When compiling from strings, this call is necessary
     * before doing any performing */
    cs.start().unwrap();

    /* Create a new thread that will use our performance function and
     * pass in our CSOUND structure. This call is asynchronous and
     * will immediately return back here to continue code execution
     */
    let cs = Arc::new(Mutex::new(cs));
    let cs = Arc::clone(&cs);

    let child = thread::spawn(move || while !cs.lock().unwrap().perform_ksmps() {});

    child.join().unwrap();
}
