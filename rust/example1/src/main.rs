extern crate csound;
use csound::*;

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
    let mut cs = Csound::new();

    let args = ["csound", CSD];
    cs.compile(&args).unwrap();

    cs.start().unwrap();

    cs.perform();
}
