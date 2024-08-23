# Csound 6 - Java API Examples
Author: Steven Yi <stevenyi@gmail.com>
2013.11.24

This folder contains examples for using the Csound API in Java. They start with a minimal usage of Csound and each example afterwards builds upon the previous one.  There are numerous comments in each file to explain what is new in the example as well as explain how the new things may be used. 

Note: These examples were translated from the Python examples.

## Useful Notes

* Each example is written within their own Example.java file, each with their own main method.
* These examples were written using the Netbeans IDE (version 7.3.1). You can open the Java folder as a Netbeans project and choose individual Example.java files and use "Run File" to run the Example.
* Optionally, you can use ant to build the project from the commandline.  This will build and package the examples in dist/CsoundAPIExamples.jar.  You can then run individual examples using a commandline such as:

    java -cp dist/CsoundAPIExamples.jar csoundapiexamples.Example1

* The functions with the Java API do not have the same documentation as is found in the original C API or C++ API's.  It is recommended to lookup further information from the Csound header files, found in "csound6/include/csound.h" and "csound6/include/csound.hpp" respectively.

