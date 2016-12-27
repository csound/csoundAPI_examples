/* Example 8 - More efficient Channel Communications
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example builds on Example 7 by replacing the calls to SetControlChannel
 * with using csoundGetChannelPtr. In the Csound API, using SetControlChannel 
 * and GetControlChannel is great for quick work, but ultimately it is slower 
 * than pre-fetching the actual channel pointer.  This is because 
 * Set/GetControlChannel operates by doing a lookup of the Channel Pointer, 
 * then setting or getting the value.  This happens on each call. The 
 * alternative is to use csoundGetChannelPtr, which fetches the Channel Pointer 
 * and lets you directly set and get the value on the pointer.
 *
 * One thing to note though is that csoundSetControlChannel is protected by 
 * spinlocks.  This means that it is safe for multithreading to use.  However,
 * if you are working with your own performance-loop, you can correctly process 
 * updates to channels and there will be no problems with multithreading.
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <csound.h>
#include <string.h>
#include <time.h>

typedef struct _random_line {
    int dur;
    double end;
    double increment;
    double current_val;
    double base;
    double range;
} random_line ;


/* Resets a random_line by calculating new end, dur, and increment values */
void random_line_reset(random_line* rline) {
    rline->dur = (rand() % 256)  + 256;
    rline->end = (double)rand() / RAND_MAX;
    rline->increment = (rline->end - rline->current_val) / rline->dur;
}

/* Creates a random_line and initializes values */
random_line* random_line_create(double base, double range) {
    random_line* retval = (random_line*)calloc(1, sizeof(random_line));
    retval->base = base;
    retval->range = range;

    random_line_reset(retval);

    return retval;
} 

/* Advances state of random line and returns current value */
double random_line_tick(random_line* rline) {
    double current_value = rline->current_val;
    rline->dur -= 1;
    if(rline->dur <= 0) {
        random_line_reset(rline);
    } 
    rline->current_val += rline->increment;
    return rline->base + (current_value * rline->range);
}

/* Defining our Csound ORC code within a multiline String */
const char* orc = "sr=44100\n"
  "ksmps=32\n"
  "nchnls=2\n"
  "0dbfs=1\n\n"
  "instr 1\n"
  "kamp chnget \"amp\"\n"
  "kfreq chnget \"freq\"\n"
  "printk 0.5, kamp\n"
  "printk 0.5, kfreq\n"
  "aout vco2 kamp, kfreq\n"
  "aout moogladder aout, 2000, 0.25\n"
  "outs aout, aout\n"
  "endin";


int main(int arg, char** argv) {
    random_line *amp, *freq;
    MYFLT *amp_channel, *freq_channel;

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
    csoundReadScore(csound, "i1 0 60");

    /* When compiling from strings, this call is necessary 
    * before doing any performing */
    csoundStart(csound);

    /* Create a random_line for use with Amplitude */
    amp = random_line_create(0.4, 0.2);

    /* Create a random_line for use with Frequency */
    freq = random_line_create(400.0, 80.0);

    /* Retrieve Channel Pointers from Csound */
    csoundGetChannelPtr(csound, &amp_channel, "amp", 
        CSOUND_CONTROL_CHANNEL | CSOUND_INPUT_CHANNEL);
    csoundGetChannelPtr(csound, &freq_channel, "freq", 
        CSOUND_CONTROL_CHANNEL | CSOUND_INPUT_CHANNEL);
    
    /* Initialize channel values before running Csound */
    *amp_channel = random_line_tick(amp);
    *freq_channel = random_line_tick(freq);

    /* The following is our main performance loop. We will perform one 
    * block of sound at a time and continue to do so while it returns 0, 
    * which signifies to keep processing.  
    */
    while (csoundPerformKsmps(csound) == 0) {
        /* Update Channel Values by directly setting memory */
        *amp_channel = random_line_tick(amp);
        *freq_channel = random_line_tick(freq);
    }

    csoundStop(csound);

    return 0;
}

