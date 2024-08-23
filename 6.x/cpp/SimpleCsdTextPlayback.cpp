/* Simple Compilation with csoundCompileCsdText() */

#include <stdio.h>
#include "csound.hpp"
#include <string>

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

//prepare Csound for performance
csound->Start();

//perform entire score
csound->Perform();	

//free Csound object
delete csound;

return 0;
}



