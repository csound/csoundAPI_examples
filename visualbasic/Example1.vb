' All Examples:
' Author: Steven Yi <stevenyi@gmail.com>
' 2013.10.28 
' Adapted to Visual Basic by Richard Henninger <richard@rghmusic.com> using
' a .net bridge to csound available at: https://csound6net.codeplex.com/

' Unlike the Python-based examples, the VB examples, like the C# examples
' are implemented as individual methods on a single class and executed
' from a Main Module console program that accepts an example's number to execute it.
' The class is implemented as a partial class to keep directory organization
' similar looking to the Python and C# examples.

' All VB examples are enclosed in Try/Catch/Finally blocks which insures proper
' release of system resources via the IDispose interface which the bridge supports
' even if an exception occurs.
' This provides the same orderly shutdown of csound enjoyed by C#'s "using" blocks.

' The first thing we do, assuming that csound 6.01 has been installed along
' with csound6netlib.dll, is add a project reference to csound6netlib.dll
' (further references to csound will be automatic if the environment PATH
' variable leads to csound itself) and to add the "Imports csound6netlib" statement
' to bring the library into our project.
' csound6netlib.dll is used as the bridge between .net managed code
' and csound's unmanaged "c" API's code.

' Remember to add an Imports statement so intelliSense and your code
' find csound 6's classes and methods.


Partial Public Class Examples

    ' Example 1 - Simple Compilation with Csound

    ' This is a barebones example for creating an instance of Csound, 
    ' compiling a pre-existing CSD, calling Perform to run Csound to completion,
    ' then Stop and exit.  
    ' Resulting wave file will be found in the current directory or in SFDIR if established

    Public Sub Example1()
        'Create an instance of the Csound object 
        Using c As New Csound6Net

            'Compile a pre-defined test1.csd file, includes Start()
            c.Compile(New String() {"test1.csd"})

            'This call runs Csound to completion (saving Stop() for next example)
            c.Perform()

        End Using

    End Sub
End Class
