using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using csound6netlib;  //Exposes DotNet to Csound 6 Bridge's classes from csound6netlib.dll

namespace csoundAPI_examples
{
    public partial class CsoundAPI_Examples
    {
       /* Example 3 - Using our own performance loop
        * 
        * In this example, we use a while loop to perform Csound one audio block at a time.
        * This technique is important to know as it will allow us to do further processing
        * safely at block boundaries.  We will explore the technique further in later examples.
        * 
        * The C# version includes checking the status code which csound returns after key processing.
        * These codes are encapsulated in the CsoundStatus enumeration.
        * This demonstrates the traditional way to respond to csound compiling and performing
        */
        public void Example3()
        {
            const string sco = "i1 0 1\n";
            using (var c = new Csound6Net())
            {
                //You can also set the output file or device using SetOutputFileName method in the API
                // SetOutputFileName(string path, SoundFileType type, SampleFormat format)
                // or its convenience method for real time output SetOutputDac(int dacNbr)
                // instead of via command line arguments fed to SetOption(string option) one at a time
                c.SetOutputDac(0);

                var status = c.CompileOrc(orc);      // Compile Orchestra from String
                if (status == CsoundStatus.Success)  // Classic csound practice tests compile results before proceeding 
                {
                    status = c.ReadScore(sco);       // Read in Score from String variable
                    if (status == CsoundStatus.Success)
                    {
                        c.Start(); // When compiling from strings, this call needed before performing

                        /* The following is our main performance loop.
                         * We will perform one block of sound at a time and continue to do so
                         * while it returns false, which tells us to keep processing.
                         * We will explore this loop technique in further examples.
                         */
                        while (!c.PerformKsmps());

                        c.Stop();
                    }
                }
            }
        }


    }
}
