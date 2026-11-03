/* Example 1 - Playing a CSD with the Csound 7 API
 * Author: Steven Yi <stevenyi@gmail.com>
 * Updated for Csound 7 / csound-rs 0.3 by Natanael Mojica <neithanmo@gmail.com>
 *
 * This first example shows the minimal amount of code required to run
 * Csound from Rust: hand it a complete CSD and let it perform to the end
 * of the score.
 *
 * The CSD here is kept in an in-memory string and compiled with
 * `compile_csd_text(.., Dispatch::Sync)`. `start()` must be called before performing, and
 * `perform_ksmps()` renders one block of audio at a time, returning `true`
 * once the score is finished.
 */

use csound::{Csound, Dispatch};

static CSD: &str = "<CsoundSynthesizer>
<CsOptions>
-odac
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 32
nchnls = 2
0dbfs  = 1
instr 1
kamp = .6
kcps = 440
ifn  = p4
asig oscil kamp, kcps, ifn
     outs asig,asig
endin
</CsInstruments>
<CsScore>
f1 0 16384 10 1
i 1 0 2 1
e
</CsScore>
</CsoundSynthesizer>";

fn main() {
    let cs = Csound::new().expect("failed to create a Csound instance");

    // Compile the CSD from the in-memory string.
    cs.compile_csd_text(CSD, Dispatch::Sync).unwrap();

    // Required before performing.
    cs.start().unwrap();

    // Render one block at a time until the score ends.
    while !cs.perform_ksmps() {}
}
