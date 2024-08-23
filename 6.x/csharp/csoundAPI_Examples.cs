using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

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
         * Exercise number can be an integer or, if there are subtests, a real number
         * where the fractional digit is the one-based subtest number (like 5.2 to
         * run the third subtest in example 5 or 5.0 to run the first)
         */
        static void Main(string[] args)
        {
            var pgm = new CsoundAPI_Examples();
            double choice = 0.0;
            do
            {
                Console.Write("\nEnter a test number between 1 and 9 (can be real number for subtests): ");
                string val = Console.ReadLine();
                if (double.TryParse(val, out choice))
                {
                    int pgmNbr = (int)choice;
                    int subPgmNbr = (((int)(choice * 10)) % 10);
                    switch (pgmNbr)
                    {
                        case 1: pgm.Example1(); break;
                        case 2: pgm.Example2(); break;
                        case 3: pgm.Example3(); break;
                        case 4:
                            if ((subPgmNbr % 2) == 0)
                                pgm.Example4();//use performanceThread
                            else
                            {
                    //            var t = pgm.Example41(); //use C# Tasks: threading issues still
                    //            Task.WaitAll(t);
                            }
                            break;
                        case 5: pgm.Example5(subPgmNbr % 3); break;
                        case 6: pgm.Example6(); break;
                        case 7: pgm.Example7(); break;
                        case 8: pgm.Example8(); break;
                        case 9: pgm.Example9(); break;
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
