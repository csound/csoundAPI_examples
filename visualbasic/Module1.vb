Imports System


' All Examples:
' Author: Steven Yi <stevenyi@gmail.com>
' 2013.10.28 
' Adapted to Visual Basic by Richard Henninger <richard@rghmusic.com> using
' a .net bridge to csound available at: https://csound6net.codeplex.com/

' Unlike the Python-based examples, the VB examples are implemented
' as individual methods on a single class and executed from a static main
' console program that accepts an example's number to execute it.
' This makes it easy to run from within Visual Studio as a single executable.
' The class is implemented as a partial class to so the examples can
' appear separately like the Python examples while still remaining a single class.
' The support objects (Note, RandomLine) are in separate class files as well.
' The reusable orchestra strings are defined as constants within the class below.
' 
' All VB examples are enclosed in a Try/Catch/Finally block which insures proper
' release of system resources via the IDispose interface which the bridge supports.
' This makes sure basic Csound housekeeping are automatically handled even when
' an exception is thrown.
' The VB constructor initializes and creates the Csound structure while the IDispose
' implementation (with the destructor) makes sure cleanup and destroy are called.

' The first thing we do, assuming that csound 6.01 has been installed along
' with csound6netlib.dll, is add a project reference to csound6netlib.dll
' (further references to csound will be automatic if the environment PATH
' variable leads to csound itself) and to add the "Imports csound6netlib" statement
' to bring the library into our project.
' csound6netlib.dll is used as the bridge between .net managed code
' and csound's unmanaged "c" API's code.


Public Module Module1


    'This is a simple console program to run the examples.
    'The examples themselves are in their own file as are support classes
    ' used in several examples.
    'The orchestra strings are part of this class and defined below.

    'When runnings, enter an exercise number to hear the result.
    'Exercise number can be an integer or, if there are subtests, a real number
    ' where the fractional digit is the one-based subtest number (like 5.2 to
    ' run the third subtest in example 5 or 5.0 to run the first)

    Sub Main()
        Dim pgm As New Examples()
        Dim choice = 0.0
        Dim pgmNbr, subPgmNbr As Integer

        Do
            Console.Write("Enter a test number between 1 and 9 (can be real number for subtests): ")
            Dim val = Console.ReadLine()
            If (Double.TryParse(val, choice)) Then

                pgmNbr = choice
                Select Case pgmNbr

                    Case 1
                        pgm.Example1()
                    Case 2
                        pgm.Example2()
                    Case 3
                        pgm.Example3()
                    Case 4
                        subPgmNbr = (((choice * 10)) Mod 10)
                        If (subPgmNbr Mod 2) = 0 Then
                            pgm.Example4()
                        Else
                            '       Dim t As Task = pgm.Example41()
                            '      Task.WaitAny(t)
                        End If

                    Case 5
                        subPgmNbr = (((choice * 10)) Mod 10)
                        pgm.Example5(subPgmNbr)
                    Case 6
                        pgm.Example6()
                    Case 7
                        pgm.Example7()
                    Case 8
                        pgm.Example8()
                    Case 9
                        pgm.Example9()
                    Case Else
                        choice = -1.0
                End Select
            End If
       Loop While choice > 0.0
    End Sub

End Module
