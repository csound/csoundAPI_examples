#include <stdio.h>
#include <csound.h>

/**
 * TODO - Add comments!
 */

int main(int arg, char** argv) {

  csoundInitialize(CSOUNDINIT_NO_ATEXIT);

  CSOUND* csound = csoundCreate(NULL);
  char* args[2];
  args[0] = "csound";
  args[1] = "test1.csd";

  csoundCompile(csound, 2, args);  
  csoundPerform(csound);
  csoundStop(csound);

  return 0;
}
