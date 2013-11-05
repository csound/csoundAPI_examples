using System;
using System.Collections.Generic;
using System.Text;

/*
 * All Examples:
 * Author: Steven Yi <stevenyi@gmail.com>
 * 2013.10.28 
 * Adapted to C# by Richard Henninger <richard@rghmusic.com> using
 * a .net bridge to csound available at: https://csound6net.codeplex.com/
 * 
 * Unlike the Python-based examples, the C# examples are implemented
 * as individual methods on a single class and executed from a static main
 * console program that accepts an example's number to execute it.
 * This makes it easy to run from within Visual Studio as a single executable.
 * The class is implemented as a partial class to so the examples can
 * appear separately like the Python examples while still remaining a single class.
 * The support objects (Note, RandomLine) are in separate class files as well.
 * The reusable orchestra strings are defined as constants within the class below.
 * 
 * All C# examples use the "using" block convention which insures proper
 * release of system resources via the IDispose interface which the bridge supports.
 * This makes sure basic Csound housekeeping are automatically handled.
 * The C# constructor initializes and creates the Csound structure while the IDispose
 * implementation (with the destructor) makes sure cleanup and destroy are called.
 * 
 * The first thing we do, assuming that csound 6.01 has been installed along
 * with csound6netlib.dll, is add a project reference to csound6netlib.dll
 * (further references to csound will be automatic if the environment PATH
 * variable leads to csound itself) and to add the "using csound6netlib" statement
 * to bring the library into our project.
 * csound6netlib.dll is used as the bridge between .net managed code
 * and csound's unmanaged "c" API's code.
 */

using csound6netlib;  //Exposes DotNet to Csound 6 Bridge's classes from csound6netlib.dll

namespace csoundAPI_examples
{

    public partial class CsoundAPI_Examples
    {
        /*
         * This is a simple console program to run the examples.
         * The examples themselves are in their own file as are support classes
         * used in several examples.
         * The orchestra strings are part of this class and defined below.
         * 
         * When runnings, enter an exercise number to hear the result.
         * Exercise 5 randomly selects one of three note-generating algorithms.
         * Run it repeatedly until you have heard all three.
         */
        static void Main(string[] args)
        {
            var pgm = new CsoundAPI_Examples();
            var r = new Random();
            int choice = 0;
            do
            {
                Console.Write("\nEnter a test number between 1 and 7: ");
                string val = Console.ReadLine();
                if (int.TryParse(val, out choice))
                {
                    switch (choice)
                    {
                        case 1: pgm.Example1(); break;
                        case 2: pgm.Example2(); break;
                        case 3: pgm.Example3(); break;
                        case 4: Console.WriteLine("Example 4 not implemented yet");  break;// pgm.Example4(); break;
                        case 5: pgm.Example5(r.Next(3)); break;
                        case 6: pgm.Example6(); break;
                        case 7: pgm.Example7(); break;
                        default:
                            choice = -1;
                            break;
                    }
                }
                else choice = 0;
            } while (choice > 0);
        }


    // Since the orc strings are re-used in many examples, we define them once
    // here as constants rather than repeatedly in each example.

//Used in examples 2, 3 and 4
const string orc = @"
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
aout vco2 0.5, 440
outs aout, aout
endin";


//Used in example 5 and 6
const string orc2 = @"
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
ipch = cps2pch(p5, 12)
kenv linsegr 0, .05, 1, .05, .7, .4, 0
aout vco2 p4 * kenv, ipch 
aout moogladder aout, 2000, 0.25
outs aout, aout
endin
";


// Used in examples 7, 8 and 9
const string orc3 = @"
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
kamp chnget ""amp""
kfreq chnget ""freq""
printk 0.5, kamp
printk 0.5, kfreq
aout vco2 kamp, kfreq
aout moogladder aout, 2000, 0.25
outs aout, aout
endin";



    }
}
