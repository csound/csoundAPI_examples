/* Example 7 

 This example attempts to show how one might embed Csound into an application that has its own 
 audio IO interface. Csound's GetSpout() and GetSpin() method let us tap directly into Csound's
 IO buffers. Note this example still uses Csound to output audio, but in most cases you should run 
 Csound with '-n' set in the CsOptions. THis will prevent Csound from trying to open any audio IO 
 devices.     

 Typical command line to build this:
 g++ example8.cpp -o test -I ../../csound/include/ -lcsound64 -lsndfile

 RoryWalsh 2016
*/

#include <stdio.h>
#include "csound.hpp"
#include <iostream>

using namespace std;

#ifdef __GNUC__
#define NOT_USED(var) var __attribute__((unused))
#endif

int main()
{
MYFLT *spin, NOT_USED(*spout);
int sampleCount=0;

//For sake of simplicity we are generating a simple waveform.
//In most cases a signal will be taken directly from your sound
//card using an audio library.  
MYFLT simpleRamp[44100];
for(int i=0;i<44100;i++)
{
	simpleRamp[i] = (float)i/44100.f;
}

//Create an instance of Csound
Csound* csound = new Csound();

//compile instance of csound.
csound->Compile("test8.csd");

//access Csound's input/output buffer
spout = csound->GetSpout();
spin  = csound->GetSpin();

//start Csound performance
csound->Start();

while(csound->PerformKsmps()==0)
{
	//this is our main processing loop. External audio can be sent 
	//to Csound in this loop by writing directly to the spin
	//buffers. Csound picks up the input signal using its audio input opcodes.
	//see test8.csd. 
	for(int i=0;i<csound->GetKsmps();i++)
	{
		spin[i] = simpleRamp[sampleCount];
		sampleCount=sampleCount==44100 ? 0 : sampleCount+1;

		//Csound's output signal can also be accessed through spout and output to 
		//a soundcard using an audio library. 
	}
}

//free Csound object
delete csound;

return 0;
}
