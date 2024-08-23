/* Example 7 - Communicating continuous values with Csound's Channel System
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example introduces using Csound's Channel System to communicate 
 * continuous control data (k-rate) from a host program to Csound. The 
 * first thing to note is random_line_create(). It takes in a base value
 * and a range in which to vary randomly.  The reset functions calculates
 * a new random target value (end), a random duration in which to 
 * run (dur, expressed as # of audio blocks to last in duration), and
 * calculates the increment value to apply to the current value per audio-block.
 * When the target is met, the random_line_tick() function will call 
 * random_line_reset() to update a new target value and duration.
 * 
 * In this example, we use two random_line's, one for amplitude and 
 * another for frequency.  We start a Csound instrument instance that reads
 * from two channels using the chnget opcode. In turn, we update the values
 * to the channel from the host program. To update the channel,
 * we call the csoundSetControlChannel function on the Csound struct, passing 
 * a channel name and value.  Note: The random_line_tick() function not only 
 * gets us the current value, but also advances the internal state by the 
 * increment and by decrementing the duration.
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

    
    /* Initialize channel values before running Csound */
    csoundSetControlChannel(csound, "amp", random_line_tick(amp));
    csoundSetControlChannel(csound, "freq", random_line_tick(freq));

    /* The following is our main performance loop. We will perform one 
    * block of sound at a time and continue to do so while it returns 0, 
    * which signifies to keep processing.  
    */
    while (csoundPerformKsmps(csound) == 0) {
        /* Update Channel Values */
        csoundSetControlChannel(csound, "amp", random_line_tick(amp));
        csoundSetControlChannel(csound, "freq", random_line_tick(freq));
    }

    csoundStop(csound);
    return 0;
}

