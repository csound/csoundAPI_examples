# Csound 7 - Rust API Examples

Author: Natanael Mojica <neithanmo@gmail.com>

This folder contains examples for using the Csound 7 API from Rust, built on the
[`csound`](https://crates.io/crates/csound) crate (0.3). They start with the
minimal use of Csound, and each example builds on the previous one. They are a
port of the Csound 6 Rust examples to the Csound 7 host API.

## Requirements

* Rust and Cargo — see [How to install](https://www.rust-lang.org/tools/install).
* A **Csound 7** development installation (headers + `libcsound64`). Csound 6 is
  not supported by the 0.3 bindings. The `csound-sys` build script finds Csound
  through `pkg-config`, the `CSOUND_INCLUDE_DIR` / `CSOUND_LIB_DIR` environment
  variables, or the standard per-platform locations. See the
  [csound-rs README](https://github.com/neithanmo/csound-rs) for details.

## Running the examples

Each example plays to your default audio device (`-odac`). To run one:

```
$ cd 7.x/rust/
$ cargo run --example example5
```

That runs example 5.

Example 11 is the Nannou user-interface example. It uses Nannou 0.20 with egui and runs the Csound engine on a separate thread:

```
$ cargo run --example example11
```

## What changed from the Csound 6 examples

The Csound 7 host API removed and renamed part of the 6.x interface, so these
ports differ from the `6.x/rust` set in a few consistent ways:

* `Csound::new()` returns a `Result`.
* `compile_orc` and `send_string_event` take a [`Dispatch`](https://docs.rs/csound/latest/csound/enum.Dispatch.html)
  value instead of a raw async flag. Use `Dispatch::Sync` for synchronous calls.
  Use `compile_csd(path, Dispatch::Sync)` for a file or
  `compile_csd_text(text, Dispatch::Sync)` for an in-memory CSD.
* `read_score` is gone. Score statements are sent as events with
  `send_string_event`. Send them **before** `start()` so the score is known up
  front and the `perform_ksmps` loop ends with the score. (Events sent after
  `start()` put Csound in real-time mode, where performance runs indefinitely.)
* `csoundPerform` is gone; drive performance with a `while !cs.perform_ksmps()`
  loop.
* Channels are strongly typed: fetch a writable control channel with
  `cs.get_input_channel::<ControlChannel>("name")` and write through a lock
  guard, `channel.lock().write(value)`. `set_control_channel` is still available
  for convenient, lookup-per-call access.

## Documentation

API reference for the bindings: <https://docs.rs/csound>.
