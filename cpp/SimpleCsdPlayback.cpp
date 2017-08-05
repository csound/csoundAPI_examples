/* Example 1 - Simple Compilation with Csound
 Author: Steven Yi <stevenyi@gmail.com> 2013.10.28
 Adapted for C++ by Rory Walsh

 Adapted to C++ by Rory Walsh 
 This example is a barebones example for creating an instance of Csound, 
 compiling a pre-existing CSD, calling Perform to run Csound to completion,
 then Stop and exit.  
*/

#include <stdio.h>
#include "csound.hpp"

int main()
{
//Create an instance of Csound
Csound* csound = new Csound();

//compile instance of csound.
csound->Compile("test1.csd");

//prepare Csound for performance
csound->Start();

//perform entire score
csound->Perform();	

//free Csound object
delete csound;

return 0;
}



