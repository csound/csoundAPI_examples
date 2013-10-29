# Csound 6 - Python API Examples
Author: Steven Yi <stevenyi@gmail.com>
2013.10.28

This folder contains examples for using the Csound API in Python. They start with a minimal usage of Csound and each example afterwards builds upon the previous one.  There are numerous comments in each file to explain what is new in the example as well as explain how the new things may be used. 

## Useful Notes

* It is recommended to use ipython when learning the Csound API.  This can be installed using:
  * easy_install ipython
* To run an example, in a terminal, type "python" followed by the example file. For example, to run example1.py, use "python example1.py".
* You can query the Csound API from a python prompt by doing "dir(csnd6)" or "help(csound)", assuming you have imported csnd6 with "import csnd6" as well as created an object using "csound = csnd6.Csound()". The use of the dir() and help() functions is highly recommended when learning the API.  
* The functions with the Python API do not have the same documentation as is found in the original C API or C++ API's.  It is recommended to lookup further information from the Csound header files, found in "csound6/include/csound.h" and "csound6/include/csound.hpp" respectively.

