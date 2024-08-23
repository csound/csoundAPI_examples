/* Example 1 - Simple Compilation with Csound
 Author: Steven Yi <stevenyi@gmail.com> 2013.10.28
 Adapted for C++ by Rory Walsh

 In this example, we will look at three techniques for generating our Score. 
 
 The first is one(SCO EXAMPLE1 ) we have already seen, which is to just write out the score
 by hand as a String.

 Knowing that we pass strings into Csound to pass note events, we can also
 generate the string.  In the second example(SCO EXAMPLE 2), sco2 starts as an empty string.
 Using a for-loop, we append to sco2 note strings using the << operators.  
 The values are calculated using i which is the index of our for loop. The reslt of all this 
 is a score that represents an ascending note line. 

 In the final example(SCO EXAMPLE 3), we are going to generate a vector of 13 note values.  
 We then randomly choose what order to have the notes played back by randomly choosing 
 elements from the vector and adding it to our sco3 string.  
 The end result is a sequence of randomly selected notes from our note vector.

 The final example represents a common pattern of development. For systems that
 employ some event-based model of music, it is common to use some kind of data
 structure to represent events.  This may use some kind of common data structure
 like a list, or it may be represented by using a class and instances of that
 class. 

 Note, the three examples here are indicated with comments. To listen to the examples,
 look for the lines that have csound->ReadScore(sco..) (lines 98-101), uncomment the one
 you want to hear, and comment out the others. 
*/
#include <iostream>
#include <iomanip>
#include <sstream>
#include "csound.hpp"
#include <string>
#include <vector>
#include "csPerfThread.hpp"
#include <stdlib.h>  
#include <time.h>  

using namespace std;

int main()
{
//ostringstream allows easy formatting of strings
std::ostringstream tmp;
std::string orc = "sr=44100\n\
ksmps=32\n\
nchnls=2\n\
0dbfs=1\n\
\n\
instr 1 \n\
ipch = cps2pch(p5, 12)\n\
kenv linsegr 0, .05, 1, .05, .7, .4, 0\n\
aout vco2 p4 * kenv, ipch \n\
aout moogladder aout, 2000, 0.25\n\
outs aout, aout\n\
endin";

/*
 * SCORE EXAMPLE 1
 * Creates a score using a single string 
 */
string sco1 = "i1 0 1 .25 8.00";

/*
 * SCORE EXAMPLE 2
 * Create a score string using a for loop
 */
string sco2="";
for(int i=0;i<13;i++)
	tmp  << "i1 " << i*.25 << " .25 0.5 8." << setfill('0') << setw(2) << i << endl;
sco2 = tmp.str();

/*
 * SCORE EXAMPLE 3
 * Create a vector of note which then get played
 * randomly by Csound 
*/ 
vector<string> notes;
string sco3="";	
for(int i=0;i<13;i++){
	tmp.str("");
	tmp.clear();
	tmp << " .25 0.5 8." << setfill('0') << setw(2) << i << endl;
	notes.push_back(tmp.str());
}

srand (time(NULL));
for(int i=0;i<20;i++){
	tmp.str("");
	tmp.clear();
	tmp  << "i1 " << i*.25 << notes[rand() % 13];
	sco3+=tmp.str();
}

cout << sco3;

//create an instance of Csound
Csound* csound = new Csound();
//set CsOptions
csound->SetOption("-odac");

//compile orc
csound->CompileOrc(orc.c_str());

//compile score section
csound->ReadScore(sco1.c_str());
//csound->ReadScore(sco2.c_str());
//csound->ReadScore(sco3.c_str());

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


