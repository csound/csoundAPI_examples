/* Example 4 - Using Threads
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * In this example, we use the Csound thread functions to run Csound in 
 * a separate thread. This is a common scenario where you will run
 * Csound in one thread, and doing other things in another thread 
 * (i.e. have a GUI main thread, maybe a worker thread for heavy
 * computations, etc.). 
 * 
 * The Python example used the CsoundPerformanceThread which is a
 * C++ class that uses the same C functions used in this example.
 * To note, Csound offers thread functions so that the the developer
 * won't have to worry about what thread library is used (i.e. pthreads).
 * Using Csound's thread functions helps make your code more portable 
 * between platforms.
 */


#include <stdio.h>
#include <csound.h>

// forward declaration of performance function
uintptr_t performance_function(void* data);

/* Defining our Csound ORC code within a multiline String */
const char* orc = "sr=44100\n"
  "ksmps=32\n"
  "nchnls=2\n"
  "0dbfs=1\n\n"
  "instr 1\n"
  "aout vco2 0.5, 440\n"
  "outs aout, aout\n"
  "endin";

/*Defining our Csound SCO code */
const char* sco = "i1 0 1";

int main(int arg, char** argv) {
  void* thread;

  csoundInitialize(CSOUNDINIT_NO_ATEXIT);

  CSOUND* csound = csoundCreate(NULL);

  /* Using SetOption() to configure Csound 
  Note: use only one commandline flag at a time */
  csoundSetOption(csound, "-odac");

  /* Compile the Csound Orchestra string */
  csoundCompileOrc(csound, orc);

  /* Compile the Csound SCO String */
  csoundReadScore(csound, (char*)sco);

  /* When compiling from strings, this call is necessary 
   * before doing any performing */
  csoundStart(csound);

  /* Create a new thread that will use our performance function and 
   * pass in our CSOUND structure. This call is asynchronous and 
   * will immediately return back here to continue code execution
   */
  thread = csoundCreateThread(&performance_function, (void*)csound);
 
  /* Join will wait for the other thread to complete. If we did not 
   * call csoundJoinThread(), after csoundCreateThread() returns we 
   * would immediately move to the next line, csoundStop().  That 
   * would stop Csound without really giving it time to run.
   */
  csoundJoinThread(thread);

  csoundStop(csound);

  /* clean up Csound; this is useful if you're going to reuse a Csound 
   * instance 
   */
  csoundCleanup(csound);

  return 0;
}

/* Our performance function for the thread.  This will run Csound to 
 * completion using a while-loop.  For now, our data that we are passing
 * in is just the CSOUND structure, but in later examples we will use
 * a custom data structure so that we can pass in channels and other things
 * to use for processing at runtime. 
 */
uintptr_t performance_function(void* data) {
    CSOUND* csound = (CSOUND*) data;
    while (csoundPerformKsmps(csound) == 0) {
        /* pass for now */
    }
    return 0;
}

