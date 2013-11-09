#Csound 6 - C++ API Examples
Original examples provided by Steven Yi in Python, adapted to C++ by Rory Walsh 
2013.11.24

##Differences between original examples in Python
The twelve examples presented in this folder demonstrate the same API usages as the original Python examples by Steven Yi.  However, since C++ is a completely different language, the code presented here varies a little from Steven's original examples. The biggest difference lie in how lists are dealt with. The closest things to Python lists in C++ are std::vector and std::list. Because std::vector allows direct access of elements by position it is used instead of std::list in the examples presented.    



##Compilation instruction
In order to compile thee examples you will need to link to the Csound6 library. The following command line can be used to build on Linux:
g++ example1.cpp -o example1 -I/usr/local/include/csound -L/usr/local/lib -lcsound64 -lcsnd6
