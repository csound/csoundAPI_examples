using System;
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
 * The class is implemented as a partial class to keep directory organization
 * similar looking to the Python examples.
 * 
 * All C# examples use the "using" block convention which insures proper
 * release of system resources via the IDispose interface which the bridge supports.
 * 
 * The first thing we do, assuming that csound 6.01 has been installed along
 * with csound6netlib.dll, is add a project reference to csound6netlib.dll
 * (further references to csound will be automatic if the environment PATH
 * variable leads to csound itself) and to add the "using csound6netlib" statement
 * to bring the library into our project.
 * csound6netlib.dll is used as the bridge between .net managed code
 * and csound's unmanaged "c" API's code.
 */

// Remember to add a using statement so intelliSense and your code
//  find csound 6's classes and methods.
using csound6netlib;

namespace csoundAPI_examples
{
    public partial class CsoundAPI_Examples
    {
    /* Example 1 - Simple Compilation with Csound
     * 
     * This is a barebones example for creating an instance of Csound, 
     * compiling a pre-existing CSD, calling Perform to run Csound to completion,
     * then Stop and exit.  
     * Resulting wave file will be found in the current directory or in SFDIR if established
     */
        public void Example1()
        {
            // Create an instance of the Csound object within a using block
            using (var c = new Csound6Net())
            {
                c.Compile(new string[] { "test1.csd" });  // Compile a pre-defined test1.csd file, includes Start()
                c.Perform();        // This call runs Csound to completion (saving Stop() for next example)
            }
            //c.Dispose() shuts csound down properly. It is called automatically as a "using" block exits.
        }
    }
}
