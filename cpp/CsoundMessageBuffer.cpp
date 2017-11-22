/* CsoundMessageBuffer example. Running any Csound API example from
the command line will output Csound messages, but the CsoundMessageBuffer
can be used by host application with GUIs. In this example, all messages
from the MessageBuffer that are printed to the stdout will be preceded
by CSOUND_MESSAGE:  */

#include <stdio.h>
#include "csound.hpp"
#include <string>
#include <iostream>

using namespace std;

int main()
{
//Create an instance of Csound
Csound* csound = new Csound();

string csdText = "<CsoundSynthesizer>\n"
"<CsOptions>\n"
"csound -+rtaudio=jack -odac -B4096\n"
"</CsOptions>\n"
"<CsInstruments>\n"
"sr = 44100\n"
"ksmps = 64\n    "
"nchnls = 2\n"
"0dbfs = 1\n"
"instr 1\n"
"a1 oscili 1, 300, 1\n"
"outs a1, a1\n"
"endin\n"
"</CsInstruments>\n"
"<CsScore>\n"
"f1 0 1024 10 1\n"
"i1 0 2\n"
"</CsScore>\n"
"</CsoundSynthesizer>\n";

//compile instance of csound.
csound->CompileCsdText(csdText.c_str());

//create message buffer
csound->CreateMessageBuffer (0);

//prepare Csound for performance
csound->Start();

//perform entire score and grab messages from the message buffer
while (csound->PerformKsmps() == 0)
{	
    while (csound->GetMessageCnt() > 0)
    {
        cout << "CSOUND_MESSAGE:" << csound->GetFirstMessage();
        csound->PopFirstMessage();
    }
}	

//free Csound object
delete csound;

return 0;
}



