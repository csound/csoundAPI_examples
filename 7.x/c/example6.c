/*
  Copyright (C) 2024 Victor Lazzarini

  API Examples: multi-threaded performance
  
  This file is part of Csound.

  The Csound Library is free software; you can redistribute it
  and/or modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  Csound is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Csound; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
  02111-1307 USA

*/
#if defined(__APPLE__)
#include <CsoundLib64/csound.h>
#include <CsoundLib64/csPerfThread.h>
#else
#include <csound/csound.h>
#include <csound/csPerfThread.h>
#endif

const char *code =    
  "0dbfs = 1              \n"
  "instr 1                \n"
  "icnt = 0 \n"
  "while icnt < 12 do\n"
  "schedule 2,icnt*0.25,0.3,0.1,"
  "cpsmidinn(icnt+p4)\n"
  "icnt += 1\n"
  "od\n"
  "endin                  \n"  
  "instr 2                \n"
  "a1 expon p4,p3,0.001   \n"
  "a2 oscil a1,p5         \n"
  "    out a2             \n"
  "endin                  \n";

int main(int argc, const char *argv[]) {
  /* Create the Csound engine instance */
  CSOUND *csound = csoundCreate(NULL, NULL);

  if(csound != NULL) {
    int res;
    /* enforce realtime output and suppress messages */
    csoundSetOption(csound, "-o dac -dm0");
    /* Compile code from string */
    res = csoundCompileOrc(csound, code, 0);
    if(res == CSOUND_SUCCESS) {
      char evt[64];
      CS_PERF_THREAD *perf = csoundCreatePerformanceThread(csound);
      /* Start engine */
      res = csoundStart(csound);
      /* start performance thread */
      if(res == CSOUND_SUCCESS)
        csoundPerformanceThreadPlay(perf);
      while(csoundPerformanceThreadIsRunning(perf)) {
        /* prompt for input */
        csoundMessage(csound, "Csound>");
        /* take in event from stdin, 
           use event e <t> to finish after t secs*/
        fgets(evt, 63, stdin); 
        /* send in event asychronously */
        csoundEventString(csound, evt, 1);
        /* and exit loop if requested */
        if(evt[0] == 'e') break;
      }
      /* Join thread an wait for it to finish */
      csoundPerformanceThreadJoin(perf);
      /* Destroy the thread object */
      csoundDestroyPerformanceThread(perf);
    }
    /* Destroy the engine instance */
    csoundDestroy(csound);
    
    return 0;
   }

  return -1;
}
