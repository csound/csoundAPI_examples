/* Example 3 - Using our own performance loop
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * In this example, we use a while loop to perform Csound one audio block at a time.
 * This technique is important to know as it will allow us to do further processing
 * safely at block boundaries.  We will explore the technique further in later examples. 
 */


#include <stdio.h>
#include <csound.h>

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

  /* The following is our main performance loop. We will perform one 
   * block of sound at a time and continue to do so while it returns 0, 
   * which signifies to keep processing.  We will explore this loop 
   * technique in further examples. 
   */

  while (csoundPerformKsmps(csound) == 0) {
    /* pass for now */
  }

  csoundStop(csound);

  return 0;
}

