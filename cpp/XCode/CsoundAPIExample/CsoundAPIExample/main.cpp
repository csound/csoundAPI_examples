//
//  main.cpp
//  CsoundAPIExample
//
//  Created by walshr on 05/09/2018.
//  Copyright © 2018 walshr. All rights reserved.
//

#include <iostream>
#include <stdio.h>
#include "csound.hpp"
#include <string>

using namespace std;

int main()
{
    string orc = "sr=44100\n\
    ksmps=32\n\
    nchnls=2\n\
    0dbfs=1\n\
    \n\
    instr 1\n\
    aEnv linen 1, .1, p3, .1\n\
    aout oscili 0.5*aEnv, p4\n\
    outs aout, aout\n\
    endin";
    
    string sco = "";
    
    for ( int i = 0 ; i < 10 ; i++)
    {
        sco.append("i1 " + to_string(i) + " 1 " + to_string(i*100) + "\n");
    }
    
    cout << sco.c_str();
    
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
    csound->Perform();
    
    //free Csound object
    delete csound;
    
    return 0;
}
