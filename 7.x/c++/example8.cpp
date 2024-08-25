/*
  Copyright (C) 2024 Victor Lazzarini

  API Examples: sending signals to main input
  
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
#include <CsoundLib64/csound.hpp>
#else
#include <csound/csound.hpp>
#endif

const char *code = R"orc(
0dbfs = 1
instr 1
 asig in
 amod powoftwo asig
 a1 expon p4,p3,0.001
 a2 oscil a1, p5*amod
    out a2
endin
icnt = 0
while icnt <= 12 do
 schedule 1, icnt*0.25, 0.3, 0.5, cpsmidinn(60+icnt)
 icnt += 1
od
event_i "e", icnt*0.25
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
      MYFLT *spin, si = 1./csound.GetSr();
      double ph = 0.; 
      int nsmps = csound.GetKsmps();
      /* Start engine */
      res = csound.Start();
      /* get spin pointer */
      spin = csound.GetSpin();
      /* compute audio blocks */
      while(res == CSOUND_SUCCESS) {
        /* compute 1Hz sine modulation input signal */
        for(i = 0; i < nsmps; i++) {
          spin[i] = sin(2*M_PI*ph);
          ph += si;
          ph -= (int) ph;
        }
        res = csound.PerformKsmps();
      }
    }
  }
  return 0;
}
