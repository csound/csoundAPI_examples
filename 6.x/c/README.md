# Csound 6 - C API Examples
Author: Steven Yi <stevenyi@gmail.com>
2013.11.18

This folder contains examples for using the Csound API in C, translated from the original Python examples. They start with a minimal usage of Csound and each example afterwards builds upon the previous one.  There are numerous comments in each file to explain what is new in the example as well as explain how the new things may be used. 

Note: Examples 11 and 12 are missing as of this time until a cross-platform C GUI library can be determined.

## Useful Notes

* To build the examples, you will need to use CMake to generate the appropriate build files (i.e. XCode project, Makefile, etc.).  The build will search for the csound library and headers. If it can not find them, CMake will stop and report an error.  You may need to modify your default search paths so that CMake can find these files.

* Once CMake is run successfully, run your build system (i.e. "make"). One example executable will be compiled per example file.  

* To run the example, simple run the example executable from the command-line, i.e "./example5" would run example 5 on OSX or Linux. 
