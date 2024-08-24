/*
  Copyright (C) 2024 Victor Lazzarini

  API Examples: control channels
  
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
#include <stdio.h>
#if defined(__APPLE__)
#include <CsoundLib64/csound.h>
#else
#include <csound/csound.h>
#endif

const char *code =    
  "0dbfs = 1              \n" 
  "instr 1                \n"
  "kp chnget \"pitch\"    \n"
  "a1 expon p4,p3,0.001   \n"
  "a2 oscil a1,cpsmidinn(p5)*kp\n"
  "    out a2             \n"
  "endin                  \n";

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
        char evt[64];
        MYFLT dur = 5.;
        MYFLT pitch = 1., incr = 1./(dur*csoundGetKr(csound));
        /* Start engine */
        res = csoundStart(csound);
        /* send realtine event, synchronously */
        snprintf(evt, 64, "i1 0 %f 0.1 60\n", dur);
        csoundEventString(csound, evt, 0);
       /* compute audio blocks */
        while(res == CSOUND_SUCCESS) {       
          csoundSetControlChannel(csound, "pitch", pitch);
          res = csoundPerformKsmps(csound);
          pitch += incr;
          if(pitch > 2) 
           csoundEventString(csound, "e 0", 0);
        }
      }
    }
    /* Destroy the engine instance */
    csoundDestroy(csound);
    return 0;
  }
  return -1;
}
