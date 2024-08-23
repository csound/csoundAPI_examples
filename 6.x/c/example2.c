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

  /* Run Csound to completion */
  csoundPerform(csound);

  csoundStop(csound);

  return 0;
}


