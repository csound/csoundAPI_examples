# Csound 6 - Rust API Examples
Author: Natanael Mojica <neithanmo@gmail.com>
2019.05.11

This folder contains examples for using the Csound API in Rust. They start with a minimal usage of Csound and each example afterwards builds upon the previous one.
## Useful Notes

* It is assumed that you have installed Cargo, Rust and the csound libraries and API.
For instructions about How install rust and tools, check the documentation for [How to install](https://www.rust-lang.org/tools/install)
### Examples 1 to 10
These examples show the basic Csound usage.
To run one of the examples 1 to 10:
```
$ cd rust/
$ cargo run --example example5
```
This will run the example 5.
### Example 11 - User Interface
![](https://i.imgur.com/HZo07zU.gif)
The example 11 is intended to show how csound-rs can interact with other libraries, in this case a creative-coding framework called *nannou*.
To run this example, just:
```
$ cd example11/
$ cargo run
```

* The csound-rs bindings documentations can be found [here](https://neithanmo.github.io/csound-rs/csound/)
and how to build the csound [bindings](https://crates.io/crates/csound)
