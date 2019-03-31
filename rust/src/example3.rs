/* Example 3 - Using our own performance loop
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * In this example, we use a while loop to perform Csound one audio block at a time.
 * This technique is important to know as it will allow us to do further processing
 * safely at block boundaries.  We will explore the technique further in later examples.
 */

extern crate csound;
use csound::*;

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
static sco: &str = "i1 0 1";

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

    /* The following is our main performance loop. We will perform one
     * block of sound at a time and continue to do so while it returns 0,
     * which signifies to keep processing.  We will explore this loop
     * technique in further examples.
     */
    while cs.perform_ksmps() == false { /* pass for now */ }

    cs.stop();
}
