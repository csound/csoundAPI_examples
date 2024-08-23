/* GetStringChannel
 This example grabs a string from an instance of Csound and prints
*/

#include <stdio.h>
#include "csound.hpp"
#include "csPerfThread.hpp"
#include <iostream>

using namespace std;


#include <stdio.h>
#include "csound.hpp"

int main()
{
//Create an instance of Csound
Csound* csound = new Csound();

//compile instance of csound.
csound->Compile("test2.csd");

//prepare Csound for performance
csound->Start();

char tmp_string[4096] = {0};

//perform entire score
while(csound->PerformKsmps()==0)
{
    csound->GetStringChannel ("stringChannel", tmp_string);
    cout << tmp_string << " ";
}	

//free Csound object
delete csound;

return 0;
}