#Csound 6 - C++ API Examples

**Examples**

[SimpleCsdPlayback.cpp](SimpleCsdPlayback.cpp): Illustrates how to create an instance of the Csound class and start playback of a .csd 

[StringOrcScoPlayback.cpp](StringOrcScoPlayback.cpp): Shows how orchestras and scores can be passed as strings to an instance of Csound.

[csPerfThreadExample.cpp](csPerfThreadExample.cpp): Provides a simple example of how the csPerfThread class can be used to launch Csound in a new thread

[GenerativeScoreExample.cpp](GenerativeScoreExample.cpp): Demonstrates how a score file can be generating using simple random processes.

[SetChannelExample.cpp](SetChannelExample.cpp): Shows how SetChanel() can be used to pass information directly to an instance of Csound in realtime. 

[GetSpoutExample.cpp](GetSpoutExample.cpp): GetSpin() and GetSpout() can be used to access Csound's IO buffers. This can be useful when using the Csound library purely as a processing engine. This example shows how easy it is to pass an audio signal directly to Csound for processing.


##Compilation instruction
In order to compile thee examples you will need to link to the Csound6 library. The following command line can be used to build on Linux:
g++ SimpleCsdPlayback.cpp -o example1 -I/usr/local/include/csound -lcsound64 

### Compilation instructions for Ubuntu with apt-get

In order to compile the examples on Ubuntu, `csound.hpp` is needed. It can be obtained with<br>
`sudo apt-get install libcsnd-dev`

`csound.hpp` is then found in `/usr/include/csound`, so the compiler must be passed with the<br>
`-I/usr/include/csound`
option:<br>
`g++ -I/usr/include/csound SimpleCsdPlayback.cpp -oSimpleCsdPlayback`

`csPerfThreadExample.cpp`, `GenerativeScoreExample.cpp` and `SetChannelExample.cpp` additionaly needs to be linked with `libcsnd6`, for example like so:<br>
`g++ -I/usr/include/csound/  csPerfThreadExample.cpp -lcsound64 -lcsnd6 -ocsPerfThreadExample`
