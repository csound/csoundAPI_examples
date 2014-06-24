// Example 1 - Simple Compilation with Csound
// Adapted for golang by Francois Pinot <fggpinot@gmail.com>, 2013.11.15,
// from the original python example by Steven Yi <stevenyi@gmail.com>
//
// This example is a barebones example for creating an instance of Csound,
// compiling a pre-existing CSD, calling Perform to run Csound to completion,
// then Stop and exit.

// The first thing we do is import the csnd6 module, which is the module
// containing the Python interface to the Csound API.

package main

import "github.com/fggp/go-csnd6"

func main() {
	args := []string{"example01.go", "test1.csd"}
	// The Compile method will take a slice of strings as argument
	// whose first element is the program name like in the C API

	c := csnd6.Create(nil) // Create an instance of the Csound object
	c.Compile(args)        // Compile a pre-defined test1.csd file
	c.Perform()            // This call runs Csound to completion
	c.Stop()               // At this point, Csound is already stopped,
	//                        but this call is here as it is something that
	//                        you would generally call in real-world contexts
}
