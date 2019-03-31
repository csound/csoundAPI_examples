/* Example 2 - Compilation with Csound without CSD
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * In this example, we move from using an external CSD file to
 * embedding our Csound ORC and SCO code within our Python project.
 * Besides allowing encapsulating the code within the same file,
 * using the CompileOrc() and CompileSco() API calls is useful when
 * the SCO or ORC are generated, or perhaps coming from another
 * source, such as from a database or network.
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
    let cs = Csound::new();

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

    /* Run Csound to completion */
    cs.perform();

    cs.stop();
}
