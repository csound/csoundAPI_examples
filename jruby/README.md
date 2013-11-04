# Using the Csound API with JRuby
Author: Dave Seidel <dave.seidel@gmail.com>

Translated and adapted from Steven Yi's Csound API examples in Python.

## Prequisites

- Java Runtime Environment 1.6 or higher
- JRuby 1.7.4 or higher.
- Csound 6.00 or higher.

JRuby must be on your path. That is, you should be able to run the "jruby" and
"jirb" programs at a command prompt.

## Setup

JRuby needs to be able to find the Csound Java API and the Csound runtime. To
do this, assign the location of the API to the "java.lang.classpath" system
property and the location of the runtime to the "java.library.path" system
property, and set it all using the \_JAVA\_OPTIONS environment/shell variable.

On an Ubuntu system, add the following to the file ~/.bash_profile:

  export \_JAVA\_OPTIONS="-Djava.class.path=/usr/lib -Djava.library.path=/usr/lib/jni"

On a Windows system, use:

  \_JAVA\_OPTIONS=-Djava.class.path="C:\Program Files(x86)\Csound6\bin" -Djava.library.path="C:\Program Files(x86)\Csound6\bin"

## Notes

*TODO*: Verify correctness of Windows paths.

1. If you build Csound yourself and install it using "make install", then use
"/usr/local/lib" instead of "/usr/lib" for your classpath.

2. The method shown for Ubuntu ought to work on Linux systems other than
Ubuntu, and possibly on OS X as well.

