/* Example 1 - Simple Compilation with Csound
 Author: Steven Yi <stevenyi@gmail.com> 2013.10.28
 Adapted for C++ by Rory Walsh

 In this example, we use a CsoundPerformanceThread to run Csound in 
 a native thread.  Using a native thread is important to get the best
 runtime performance for the audio engine.  It is especially important
 for languages such as Python that do not have true native threads
 and that use a Global Interpreter Lock. CsoundPerformanceThread has
 some convenient methods for handling events, but does not have
 features for doing regular processing at block boundaries.  In general,
 use CsoundPerformanceThread when the only kinds of communication you
 are doing with Csound are through events, and not using channels.
*/

#include <stdio.h>
#include "csound.hpp"
#include <string>
#include "csPerfThread.hpp"

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

//compile orc
csound->CompileOrc(orc.c_str());

//compile sco
csound->ReadScore(sco.c_str());

//prepare Csound for performance
csound->Start();

//set up CsoundPerfThread object 
CsoundPerformanceThread* perfThread = new CsoundPerformanceThread(csound); 

//start Csound performance
perfThread->Play();

//keep the application running while performance is ongoing
while(perfThread->GetStatus() == 0);
                       	
//free Csound and thread objects
delete csound;
delete perfThread;

return 0;
}



