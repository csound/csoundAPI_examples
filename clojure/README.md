# Csound 6 - Clojure Examples
Author: Steven Yi <stevenyi@gmail.com>
2013.11.03

This folder contains examples for using the Java version of the Csound API from Clojure. They start with a minimal usage of Csound and each example afterwards builds upon the previous one.  There are numerous comments in each file to explain what is new in the example as well as explain how the new things may be used. 

## Useful Notes

* To run an example, in a terminal, if you have a clj script or something similar that will allow running .clj files,  you can use that with the .clj example. For example, to run example1.clj, use "clj example1.clj". If you do not have a clj script, you can use the clojure jar with the java command,  i.e. "java -cp clojure-1.5.1.jar clojure.main example1.clj". 
* The functions with the Java API do not have the same documentation as is found in the original C API or C++ API's.  It is recommended to lookup further information from the Csound header files, found in "csound6/include/csound.h" and "csound6/include/csound.hpp" respectively.

