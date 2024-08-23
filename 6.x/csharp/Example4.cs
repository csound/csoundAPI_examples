using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

using csound6netlib;

namespace csoundAPI_examples
{
    public partial class CsoundAPI_Examples
    {
   /**
    * Example 4 - Using Csound's Performance Thread
    * Example 4.1 - Using Csound in a C# async/await Task for threaded execution
    * 
    * In this example, we use a CsoundPerformanceThread to run Csound in 
    * a native thread.  Using a native thread is important to get the best
    * runtime performance for the audio engine.
    * CsoundPerformanceThread has some convenient methods for handling events,
    * but does not have features for doing regular processing at block boundaries.
    * In general, use CsoundPerformanceThread when the only kinds of communication you
    * are doing with Csound are through events, and not using channels.
    * 
    * Since VS2012, C# programmers have become comfortable with the async/await Task-based
    * paradigm for running background processes.
    * Example 4.1 shows an alternative to running csound in a separate thread (really Task)
    * to achieve the same result as example 4.
    * This approach is very useful in a GUI where a cancel event and a progress dialog
    * would be desireable.
    * This example bypasses these features; later examples will use them.
    */

        public void Example4()
        {
            using (var c = new Csound6NetRealtime())
            {
                c.SetOutputDac(0);      // Set realtime output for Csound
                c.CompileOrc(orc);      // Compile Orchestra from String
                c.ReadScore("i1 0 1");  // Read in Score from String

                c.Start();              // When compiling from strings, this call needed before performing

                // Create a new CsoundPerformanceThread, passing in the Csound object
                var t = new Csound6PerformanceThread(c);
                t.Play();   // starts the thread, which is now running separately from the main thread. This 
                // call is asynchronous and will immediately return back here to continue code
                // execution.
                t.Join();   // Join will wait for the other thread to complete. If we did not call Join(),
                // after t.Play() returns we would immediate move to the next line, c.Stop(). 
                // That would stop Csound without really giving it time to run. 
                c.Stop();   // stops Csound
                c.Cleanup();// clean up Csound; this is useful if you're going to reuse a Csound instance
            }
        }

   /**
    * This example does the same thing as Example4, but does it in more idiomatic C#.
    * The PerformAsync method provides for reporting progress events and responding to
    * cancellation events, but that capability is bypassed here.
    * Later examples will show how to uses these capabilities.
    */
        public async Task Example41()
        {
            using (var c = new Csound6Net())
            {
                c.SetOption("-odac");  // Set option for Csound
                c.CompileOrc(orc);     // Compile Orchestra from String
                c.ReadScore("i1 0 1");      // Read in Score from String

                c.Start();             // When compiling from strings, this call needed before performing

                //Perform the score in a background task until done
                //Neither a progress dialog or cancellation event is used in this call
                await c.PerformAsync(null, CancellationToken.None);//handles cleanup on exit
                
            }
        }


    }
}
