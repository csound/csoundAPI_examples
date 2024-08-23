/* Example 1 - Simple Compilation with Csound
 Author: Steven Yi <stevenyi@gmail.com> 2013.10.28
 Adapted for C++ by Rory Walsh
 
 In this example, we move from using an external CSD file to
 embedding our Csound ORC and SCO code within our source.
 Besides allowing encapsulating the code within the same file,
 using the CompileOrc() and CompileSco() API calls is useful when
 the SCO or ORC are generated, or perhaps coming from another
 source, such as from a database or network.
*/

#include <stdio.h>
#include "csound.hpp"
#include <string>

int main()
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

//compile orc.
csound->CompileOrc(orc.c_str());

//compile sco
csound->ReadScore(sco.c_str());

//prepare Csound for performance
csound->Start();

//perform entire score
csound->Perform();	

//free Csound object
delete csound;

return 0;
}



