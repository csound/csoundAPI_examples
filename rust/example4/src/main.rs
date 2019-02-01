/* Example 4 - Using Threads
* Adapted for Rust by Natanael Mojica <neithanmo@gmail.com>, 2019-01-21
* from the original C example by Steven Yi <stevenyi@gmail.com>
* 2013.10.28
*
* In this example, we use the Rust's thread API for execution csound performance function
* in another thread. Even though Csound has many useful thread functions.
* they are not implemented in this bindings yet,
* sharing data between threads could be incredibly unsafe, so, we take advantage of Rust
* Threads API which will help us to avoid data races.
*
*/

extern crate csound;
use csound::*;

use std::thread;
use std::sync::{Mutex, Arc};

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

     let child = thread::spawn( move || {
         while !cs.lock().unwrap().perform_ksmps() {
         }
     });

     child.join().unwrap();
}
