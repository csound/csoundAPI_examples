/* Example 1 - Simple Compilation with Csound
 Author: Steven Yi <stevenyi@gmail.com>
 2013.10.28

 Adapted to C++ by Rory Walsh 
 This example is a barebones example for creating an instance of Csound, 
 compiling a pre-existing CSD, calling Perform to run Csound to completion,
 then Stop and exit.  
*/

#include <stdio.h>
#include "csound.hpp"

int main(int argc, char *argv[])
{
//Create an instance of Csound
Csound* csound = new Csound;

//compile instance of csound.
csound->Compile("example1.csd");

//perform entire score
csound->Perform();	

//free Csound object
delete csound;
}



