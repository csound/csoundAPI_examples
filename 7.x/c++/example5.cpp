/*
  Copyright (C) 2024 Victor Lazzarini

  API Examples: compiling code on-the-fly
  
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
#include <CsoundLib64/csound.hpp>
#else
#include <csound/csound.hpp>
#endif

const char *code = R"orc(
0dbfs = 1
instr 1
 a1 expon p4,p3,0.001
 a2 oscil a1, p5
    out a2
endin
icnt = 0
while icnt <= 12 do
 schedule 1, icnt*0.25, 0.3, 0.1, cpsmidinn(60+icnt)
 icnt += 1
od
event_i "e", 10
)orc";


const char *perf =  R"orc(
icnt = 0
while icnt <= 12 do
 schedule 1, icnt*0.25, 0.3, 0.1, cpsmidinn(60+icnt)
 icnt += 1
od
)orc";

int main(int argc, const char *argv[]) {
  /* Create the Csound engine instance */
  Csound csound;
  int res = CSOUND_SUCCESS, i;
  /* Set options checking for any errors */
  for(i = 1; i < argc; i++)
    res += csound.SetOption(argv[i]);
  if(res == CSOUND_SUCCESS) {
    /* Compile code from string, synchronously */
    res = csound.CompileOrc(code, 0);
    if(res == CSOUND_SUCCESS) {
      char evt[64];
      MYFLT time  = 0.;
      const MYFLT incr = 1./csound.GetKr();
      /* Start engine */
      res = csound.Start();
      /* compute audio blocks */
      while(res == CSOUND_SUCCESS) {
        res = csound.PerformKsmps();
        /* count time */
        time += incr;
        /* after 2.5 seconds */
        if(time > 2.5) {
          /* compile new code, synchronously*/
          csound.CompileOrc(perf, 0);
          /* reset time */
          time -= 2.5;
        } 
      }
    }
  }
  return 0;
}
