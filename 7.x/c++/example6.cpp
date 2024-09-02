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
#include <CsoundLib64/csound.hpp>
#include <CsoundLib64/csPerfThread.hpp>
#else
#include <csound/csound.hpp>
#include <csound/csPerfThread.hpp>
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
  Csound csound;
  int res;
  /* enforce realtime output and suppress messages */
  csound.SetOption("-o dac -dm0");
  /* Compile code from string */
  res = csound.CompileOrc(code, 0);
  if(res == CSOUND_SUCCESS) {
    char evt[64];
    CsoundPerformanceThread csoundPerformanceThread(csound);
    /* Start engine */
    res = csound.Start();
    /* start performance thread */
    if(res == CSOUND_SUCCESS)
      csoundPerformanceThread.Play();
    while (csoundPerformanceThread.IsRunning()) {
      /* prompt for input */
      csound.Message("Csound>");
      /* take in event from stdin, 
         use event e <t> to finish after t secs 
      */
      fgets(evt, 63, stdin);
      /* send in event asychronously */
      csound.EventString(evt, 1);
      /* exit loop if requested */ 
      if(evt[0] == 'e') break;
    };
    /* Join thread an wait for it to finish */
    csoundPerformanceThread.Join();
  }
  return 0;
}