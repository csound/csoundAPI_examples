/*
  Copyright (C) 2024 Victor Lazzarini

  API Examples: accessing main outputs
  
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
#include <math.h>
#if defined(__APPLE__)
#include <CsoundLib64/csound.h>
#else
#include <csound/csound.h>
#endif

const char *code =    
  "0dbfs = 1              \n" 
  "instr 1                \n"
  "a1 expon p4,p3,0.001   \n"
  "a2 oscil a1,p5         \n"
  "    out a2             \n"
  "endin                  \n"
  "icnt = 0 \n"
  "while icnt < 12 do\n"
  "schedule 1,icnt*0.25,0.3,0.5,"
  "cpsmidinn(icnt+60)\n"
  "icnt += 1\n"
  "od\n"
  "event_i \"e\", icnt*0.25+0.05\n";

int main(int argc, const char *argv[]) {
  /* Create the Csound engine instance */
  CSOUND *csound = csoundCreate(NULL, NULL);

  if(csound != NULL) {
    int res = CSOUND_SUCCESS, i;
    /* Set options checking for any errors */
    for(i = 1; i < argc; i++)
      res += csoundSetOption(csound, argv[i]);
    if(res == CSOUND_SUCCESS) {
      /* Compile code from string, synchronously */
      res = csoundCompileOrc(csound, code, 0);
      if(res == CSOUND_SUCCESS) {
        MYFLT rms  = 0.;
        int nsmps = csoundGetKsmps(csound);
        const MYFLT *spout;
        /* Start engine */
        res = csoundStart(csound);
        /* get spout pointer */
        spout = csoundGetSpout(csound);
        /* compute audio blocks */
        while(res == CSOUND_SUCCESS) {
          res = csoundPerformKsmps(csound);
          /* compute output power rms */
          for(i = 0; i < nsmps; i++)
            rms = 0.01*rms + 0.99*(spout[i]*spout[i]);
          /* print output amp rms  */
          csoundMessage(csound, "rms: \t%f\n", sqrt(rms));
        }
      }
    }
    /* Destroy the engine instance */
    csoundDestroy(csound);
    return 0;
  }
    return -1;
}
