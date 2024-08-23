using System;
using System.Text;

using csound6netlib;    //Exposes DotNet to Csound 6 Bridge's classes from csound6netlib.dll

namespace csoundAPI_examples
{
    public partial class CsoundAPI_Examples
    {
        /* Example 2 - Compilation with Csound without CSD
         * 
         * In this example, we move from using an external CSD file to 
         * embedding our Csound ORC and SCO code directly in our C# project.
         * Besides allowing encapsulation of the code within the same file,
         * using the CompileOrc() and CompileSco() API calls is useful when
         * the SCO or ORC are generated, or perhaps coming from another 
         * source, such as from a database or network.
         */
        public void Example2()
        {
            using (var c = new Csound6Net())
            {
                // Using SetOption() to configure Csound: here to output in realtime
                c.SetOption("-odac");    // Note: SetOption uses only one commandline flag per call
                
                c.CompileOrc(orc);       // Compile the Csound Orchestra string
                c.ReadScore("i1 0 1\n");   // Compile the Csound score as a string constant

                c.Start();  // When compiling from strings, Start() is needed before performing
                c.Perform();// Run Csound to completion
                c.Stop();   // At this point, Csound is already stopped, but this call is here
            }               // as it is something that you would generally call in real-world 
        }                   // contexts.                    


    }
}
