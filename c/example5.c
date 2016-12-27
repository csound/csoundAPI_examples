/* Example 5 - Generating Score
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * In this example, we will look at three techniques for generating our Score. 
 * 
 * The first is one we have already seen, which is to just write out the score
 * by hand as a String.
 *
 * Knowing that we pass strings into Csound to pass note events, we can also
 * generate the string.  In the second example, generate_sco2 starts with an empty string.
 * Using a for-loop, we append to retval note strings using a string formatting
 * string that has its replacement values replaced.  The replace values are 
 * calculated using the i value, and the result is an ascending note line. 
 *
 * In the final example, we are going to generate a 2-dimensional array.  The top-level
 * array represents our score as a whole, and each sub-array within it represents
 * the data for a single note.  The values are populated and then a second step is done
 * to format the values into a string and to concatenate that value to the retval string.
 * The end result is a sequence of 13 notes with random pitches.
 *
 * The final example represents a common pattern of development.  For systems that
 * employ some event-based model of music, it is common to use some kind of data
 * structure to represent events.  This may use some kind of common data structure
 * like an array, or it may be represented by using a struct and instances of that
 * struct. 
 *
 * Note, the three examples here are indicated with comments.  To listen to the examples,
 * look for the lines that have csoundReadScore() (lines 117-123), uncomment the one
 * you want to hear, and comment out the others. 
 */


#include <stdio.h>
#include <stdlib.h>
#include <csound.h>
#include <string.h>
#include <time.h>


/* Defining our Csound ORC code within a multiline String */
const char* orc = "sr=44100\n"
  "ksmps=32\n"
  "nchnls=2\n"
  "0dbfs=1\n\n"
  "instr 1\n"
  "ipch = cps2pch(p5, 12)\n"
  "kenv linsegr 0, .05, 1, .05, .7, .4, 0\n"
  "aout vco2 p4 * kenv, ipch\n"
  "aout moogladder aout, 2000, 0.25\n"
  "outs aout, aout\n"
  "endin";

/* Example 1 - Static Score */
const char* sco = "i1 0 1 0.5 8.00";

/* Example 2 - Generating Score string with a loop*/
char* generate_example2() {
    char* retval = calloc(1024, sizeof(char));
    char note_string[80];
    for(int i = 0; i < 13; i++) {
        int n = sprintf(note_string, "i1 %g .25 .5 8.%02d\n", i * .25, i);
        strncat(retval, note_string, n);
    }
    printf("%s\n", retval);
    return retval;
}


/* Example 3 - Generating Score using intermediate data structure (array of arrays),
 * then converting to String.
 */
char* generate_example3() {
    /* Use array of 13 "notes", each with 5 p-fields */
    double values[13][5];
    char* retval = calloc(1024, sizeof(char));
    char note_string[80];

    /* Populate array */
    for(int i = 0; i < 13; i++) {
        values[i][0] = 1;
        values[i][1] = i * .25;
        values[i][2] = .25;
        values[i][3] = .5;
        values[i][4] = rand() % 15;
    }

    /* Convert array to to String */

    for(int i = 0; i < 13; i++) {
        int n = sprintf(note_string, "i%d %g %g %g 8.%02d\n", 
            (int)values[i][0], values[i][1], values[i][2], values[i][3], (int)values[i][4]);
        strncat(retval, note_string, n);
    }

    printf("%s\n", retval);
    return retval;
}

int main(int arg, char** argv) {

    /* initialize random seed: */
    srand (time(NULL));

    csoundInitialize(CSOUNDINIT_NO_ATEXIT);

    CSOUND* csound = csoundCreate(NULL);

    /* Using SetOption() to configure Csound 
    Note: use only one commandline flag at a time */
    csoundSetOption(csound, "-odac");

    /* Compile the Csound Orchestra string */
    csoundCompileOrc(csound, orc);

    /* Read in the Score from pre-written String */
    /*csoundReadScore(csound, (char*)sco);*/

    /* Read in the Score from loop-generated String */
    /*csoundReadScore(csound, generate_example2());*/

    /* Read in the Score from loop-generated String */
    csoundReadScore(csound, generate_example3());

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

