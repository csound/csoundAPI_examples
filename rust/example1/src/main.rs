use std::env;
extern crate csound;
use csound::*;

fn main() {
    /* Receive a command line argument, which is the path to a csound file*/
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        panic!("not enough arguments - please pass the path to a csound file");
    }

    /* Creates a new csound instance */
    let cs = Csound::new();

    let args = ["csound", &args[1]];

    cs.compile(&args).unwrap();
    cs.perform();
    cs.stop();
}
