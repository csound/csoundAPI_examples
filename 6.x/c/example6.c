/* Example 6 - Generating Score
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example continues on from Example 5, rewriting the example using
 * a struct called Note. This example also shows how an array of notes 
 * could be used multiple times.  The first loop through we use the notes 
 * as-is, and during the second time we generate the notes again with 
 * the same properties except we alter the fifth p-field up 4 semitones
 * and offset the start time.
 */


#include <stdio.h>
#include <stdlib.h>
#include <csound.h>
#include <string.h>
#include <time.h>

typedef struct _note {
    int instr_id;
    double start;
    double duration;
    double amplitude;
    int midi_keynum;
} Note;

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

/* Convert MIDI note numbers to Csound PCH Format */
char* midi2pch(int midi_keynum) {
    char* retval = calloc(80, sizeof(char));
    sprintf(retval, "%d.%02d", (3 + (midi_keynum / 12)), (midi_keynum % 12));
    return retval;
}

/* Example 3 - Generating Score using intermediate data structure (array of arrays),
 * then converting to String.
 */
char* generate_score() {
    /* Use array of 13 "notes", each with 5 p-fields */
    Note notes[13];
    char* retval = calloc(1024, sizeof(char));
    char note_string[80];

    /* Populate Notes */
    for(int i = 0; i < 13; i++) {
        notes[i].instr_id = 1;
        notes[i].start = i * .25;
        notes[i].duration = .25;
        notes[i].amplitude = .5;
        notes[i].midi_keynum = 60 + (rand() % 15);
    }

    /* Convert Notes to to String */
    for(int i = 0; i < 13; i++) {
        int n = sprintf(note_string, "i%d %g %g %g %s\n", 
            notes[i].instr_id,
            notes[i].start,
            notes[i].duration,
            notes[i].amplitude,
            midi2pch(notes[i].midi_keynum));
        strncat(retval, note_string, n);
    }

    /* Generate notes again transposed a Major 3rd up */
    for(int i = 0; i < 13; i++) {
        int n = sprintf(note_string, "i%d %g %g %g %s\n", 
            notes[i].instr_id,
            notes[i].start + .125,
            notes[i].duration,
            notes[i].amplitude,
            midi2pch(notes[i].midi_keynum + 4));
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

    /* Read in the Score from loop-generated String */
    csoundReadScore(csound, generate_score());

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

