/*
  Copyright (C) 2024 Victor Lazzarini

  API Examples: simple CLI frontend
  
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

int main(int argc, const char *argv[]) {
  /* Create the Csound engine instance */
  Csound csound;
  /* Compile code from command-line arguments */
  int res = csound.Compile(argc, argv);
  if(res == CSOUND_SUCCESS) {
      /* Start engine */
      res = csound.Start();
      /* compute audio blocks */
      while(res == CSOUND_SUCCESS)
        res = csound.PerformKsmps();
    }
    return 0;
}
