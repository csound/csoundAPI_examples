### Csound-API / Visual Studio 2017 project. 

This project assumes that a recent version of Csound has been installed using the standard Csound installer on Windows. Therefore it expects to find the Csound library in:

```C:\Program Files\Csound6_x64\lib```

And the Csound header files in:

```C:\Program Files\Csound6_x64\include\csound```

If you have a custom build of Csound, or have installed it to a different location, you will need to update the project options. 

> Note this project is set up to build with x64 architecture, therefore you must ensure this is the selected arch config for this project. If you wish to build for i386 you will need to first build Csound from source for that platform. 