# Common Lisp API examples

These examples are designed to work with bindings for Steel Bank Common Lisp
<http::/sbcl.org> in the csound-sbcl.lisp file.

They should work out of the box on MacOS with the
standard framework install locations.
On linux, a "libcsound64.so" symlink to libcsound
should exist in the working directory. For Windows, the code can be
edited to reflect the location of the Csound64.dll library file.

Note that due to memory usage it may be necessary to increase the
dynamic space size using ` --dynamic-space-size` (see command lines
at the top of each file).

- example1.lisp: simple CLI frontend
- example2.lisp; compiling from a string
- example3.lisp: reatlime events
- example4.lisp: control channels
- example5.lisp: compiling code on-the-fly
- example6.lisp: multithreaded performance
- example7.lisp: accessing main outputs
- example8.lisp: sending signals to main input
- example9.lisp: accessing function tables
- example10.lisp: compiling from CSD string and resetting





