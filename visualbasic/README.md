# Csound 6 - Visual Basic (.net) API Examples
Author: Richard Henninger <richard@rghmusic.com> 
2013.11.10

## Dot Net to Csound 6 Bridge
In addition to languages like java, lua, C++ and python, it is now possible to access the Csound API via managed languages from Microsoft's .net platform.  This folder contains examples which demonstrate using Csound 6 with Visual Basic in order to verify that any .net language can be used with the Bridge.  

## Adding the Bridge to your project 
To control Csound 6 via a .net language host, it is necessary to add an additional **dll** to your project: [csound6netlib.dll](http://csound6net.codeplex.com).  This **dll** contains pInvoke signatures and object wrappers that enable you to use .net idioms like *classes, properties, exceptions, events/delegates, generics, async/await and using blocks* in your host's code.  The binary download also provides Doxygen documentation of the Bridge's classes and their methods.  The Bridge will become part of a future Windows Csound release.  Currently, you must download it separately.

## Differences in Approach from Python Examples
The twelve examples presented in this folder demonstrate the same API usages as the original Python examples by Steven Yi.  However, since Visual Basic, like any .net language, is a true compiled object oriented language, the VB code is structured a little differently from Steven's original examples.  The VB examples are presented as methods in a single class (using partial class files) which can be run via a single main program.  This makes them easy to run from a single Visual Studio project as a single VB Module.  They also attempt to translate the Python code into more idiomatic VB usages while making similar points about Csound.


## Useful Notes

* These examples expect that you are running version 6.01 of Csound.
* Your PATH environment variable should find Csound 6's bin directory. The Window's Csound installer should have done this for you.
* The examples expect you to be running in Windows 7 or higher and using .net 4.5.
* Visual Studio 2012 was used to develop and test these examples.  You can download these file into your own Visual Studio project or download a full Visual Studio project from the [source code to the bridge](http://csound6net.codeplex.com/SourceControl/latest).
* You will want the latest version of the bridge (0.2.1): it contains these examples as a project as well as enhancements to the bridge inspired by developing these examples. 
* Remember to add a reference to **csound6netlib.dll** in your Visual Studio project and to add and a *Imports csound6netlib;* statement to any class which uses the bridge.
* Explore the included Doxygen documentation for an in depth look at the classes and methods that map to the canonical Csound API.
* You can browse and/or download [source code to the bridge](https://csound6net.codeplex.com/SourceControl/latest#CsoundNetLib/csoundAPI_Examples/csoundAPI_Examples.cs) to gain further insights into using it.  It is not included in the binary download.