/* Example 7 
 This example loads a Csound instrument and alters its frequency using SetChannel()

 Typical command line to build this:

 g++ example7.cpp ../../csound/interfaces/csPerfThread.cpp -o test -I ../../csound/include/ -I ../../csound/interfaces -lcsound64 -lsndfile

 RoryWalsh 2016
*/

#include <stdio.h>
#include "csound.hpp"
#include "csPerfThread.hpp"
#include <iostream>

using namespace std;

int main()
{
int freq=0;
bool shouldPlay = true;

//Create an instance of Csound
Csound* csound = new Csound();

//compile instance of csound.
csound->Compile("test8.csd");
//setup performance thread
CsoundPerformanceThread* perfThread = new CsoundPerformanceThread(csound); 

//start Csound performance
perfThread->Play();

//keep the application running while performance is ongoing and checks for incoming frequencies 
//from the user 
while(perfThread->GetStatus() == 0 && shouldPlay)
{
	cout << "Enter a frequency and press enter(enter a negative frequency to quit):";
	cin >> freq;
		
	//SetChannel() sends values to a channel called "freq". See example7.csd 
	//tp see how chnget retrieves these values
	if(freq>=0)
		csound->SetChannel("freq", freq);
	else
	{
		cout << "Stopping Csound";
		perfThread->Stop();
		shouldPlay = false;
	}
}


//free thread object
delete perfThread;
//free Csound object
delete csound;

return 0;
}
