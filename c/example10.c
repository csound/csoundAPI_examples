/* Example 10 - More efficient Channel Communications
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28
 *
 * This example continues on from Example 10 and introduces a channel_updater 
 * object. The create_channel_updater function will create and store a 
 * MYFLT* that represents a Csound Channel. Additionally, it will store and 
 * call a performance function pointer and store a void* for data to pass to 
 * that performance function.  update_channel() will use the perf_func with the
 * data pointer to get a new value and set the value within the MYFLT* channel.
 *
 * This example continues the illustration of a progression of a project.  Note 
 * that the process has changed a little bit where we now create a number of 
 * ChannelUpdater objects and store them in an array.  The array is then 
 * iterated through for updating the channel with the latest values.  In a 
 * real-world project, this kind of scenario occurs when there are n-number of 
 * items to update channels and one wants to have a flexible number that may 
 * even change dynamically at runtime. For C, one would probably, use
 * a linked-list or other data structure than an array to represent a dynamic
 * list of channel_updaters. For this example, an array was deemed sufficient
 * to illustrate the concept.
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

typedef double(*perf_func)(void*);

typedef struct _updater {
    MYFLT* channel;
    void* data;
    perf_func update_func;
} channel_updater;

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

/* Creates a Csound Channel and returns a MYFLT* */
MYFLT* create_channel(CSOUND* csound, char* channel_name) {
    MYFLT* chn;
    csoundGetChannelPtr(csound, &chn, channel_name, 
        CSOUND_CONTROL_CHANNEL | CSOUND_INPUT_CHANNEL);
    return chn;
}

/* Creates a channel_updater */

channel_updater* create_channel_updater(CSOUND* csound, 
    char* channel_name, perf_func update_func, void* data) {

    channel_updater *updater = malloc(sizeof(channel_updater));
    updater->channel = create_channel(csound, channel_name);
    updater->data = data;
    updater->update_func = update_func;

    return updater;
}

void update_channel(channel_updater* updater) {
    *updater->channel = updater->update_func(updater->data);
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

    channel_updater* channel_updaters[2];

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

    /* Create Channel Updaters */
    channel_updaters[0] = create_channel_updater(csound, 
        "amp",
        (perf_func)random_line_tick, 
        random_line_create(0.4, 0.2));
    channel_updaters[1] = create_channel_updater(csound, 
        "freq",
        (perf_func)random_line_tick, 
        random_line_create(400.0, 80.0));

    
    /* Initialize channel values before running Csound */
    for (int i = 0 ; i < 2; i++) {
        update_channel(channel_updaters[i]);
    }

    /* The following is our main performance loop. We will perform one 
    * block of sound at a time and continue to do so while it returns 0, 
    * which signifies to keep processing.  
    */
    while (csoundPerformKsmps(csound) == 0) {
        /* Update Channel Values using Channel Updaters */
        for (int i = 0 ; i < 2; i++) {
            update_channel(channel_updaters[i]);
        }
    }

    csoundStop(csound);
    free(channel_updaters[0]);
    free(channel_updaters[1]);
    return 0;
}


