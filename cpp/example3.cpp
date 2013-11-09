/* Example 1 - Simple Compilation with Csound
 Author: Steven Yi <stevenyi@gmail.com> 2013.10.28
 Adapted for C++ by Rory Walsh

 In this example, we use a while loop to perform Csound one audio block at a time.
 This technique is important to know as it will allow us to do further processing
 safely at block boundaries. We will explore the technique further in later examples.
*/

#include <stdio.h>
#include "csound.hpp"
#include <string>

int main(int argc, char *argv[])
{
std::string orc = "sr=44100\n\
ksmps=32\n\
nchnls=2\n\
0dbfs=1\n\
\n\
instr 1\n\
aout vco2 0.5, 440\n\
outs aout, aout\n\
endin";
	
std::string sco = "i1 0 1";	
	
//create an instance of Csound
Csound* csound = new Csound();
//set CsOptions
csound->SetOption("-odac");

//compile orc
csound->CompileOrc(orc.c_str());

//compile sco
csound->ReadScore(sco.c_str());

//prepare Csound for performance
csound->Start();

//perform entire score
while(csound->PerformKsmps()==0);	

//free Csound object
delete csound;

return 0;
}



