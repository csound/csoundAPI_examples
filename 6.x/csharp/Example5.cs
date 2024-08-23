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
        /* Example 5 - Generating a Score, C# try/catch blocks
         * 
         * In this example, we will look at three techniques for generating our Score. 
         * The first is one we have already seen, which is to just write out the score
         * by hand as a String.
         *
         * Knowing that we pass strings into Csound to pass note events, we can also
         * generate the string.  In the second example, sco2 starts as an empty string.
         * Using a for-loop, we append to sco2 note strings using a string formatting
         * string that has its replacement values replaced.  The replace values are 
         * calculated using the i value, and the result is an ascending note line. 
         *
         * In the final example, we are going to generate a list of lists.  The top-level
         * list represents our score as a whole, and each sub-list within it represents
         * the data for a single note.  The main list is then processed in two ways: first,
         * it processes each sub-list and joins the values together into a single note string;
         * second, it joins each individual note string into a single, large score string,
         * separated by newlines.  The end result is a sequence of 13 notes with random
         * pitches.
         *
         * The final example represents a common pattern of development.  For systems that
         * employ some event-based model of music, it is common to use some kind of data
         * structure to represent events.  This may use some kind of common data structure
         * like a list, or it may be represented by using a class and instances of that
         * class. 
         *
         * Note, the three examples here are indicated within case statement. To listen to the examples,
         * use a real number when selecting Example 5: 5.0 for the single note, 5.1 for the
         * chromatic scale and 5.2 for random pitches.
         */
        public void Example5(int algorithm)
        {
            //Choose one of 3 ways to create a score: sequential pitches, random pitches, or hard-coded
            var sco = new StringBuilder();
            switch (algorithm) //integer between 0 and 2
            {
                case 1:
                    for (int i = 0; i < 13; i++) //Chromatic Scale
                        sco.AppendLine(string.Format("i1 {0} .25 0.5 8.{1:00}", (i * .25), i));
                    break;
                case 2:
                    var vals = new List<double[]>(); //initialize a list to hold lists of doubles 
                    Random r = new Random();
                    for (int i = 0; i < 13; i++)     // populate that list with note parameters
                        vals.Add(new double[] { i * .25, .25, 0.5, r.Next(15) });
                    // convert list of lists into a list of strings with random frequencys
                    foreach (var val in vals)
                        sco.AppendLine(string.Format("i1 {0} {1} {2} 8.{3:00}", val[0], val[1], val[2], (int)val[3]));
                    break;
                case 0:
                default:
                    sco.AppendLine("i1 0 1 0.5 8.00");  //string literal with single note
                    break;
            }

            // Once a score string has been algorithmically assembled, do the usual csound loop.
            // This time, we'll let C# Exceptions (try/catch) handle testing status for us.
            // By catching the exception, we are assured exiting the "using" block
            // and thereby disposing of csound and its memory correctly.

            using (var c = new Csound6Net())
            {
                c.SetOutputDac(0);   // Set realtime output 
                try
                {
                    c.CompileOrc(orc2);         // Compile different Orchestra with moogladder 
                    c.ReadScore(sco.ToString());// Read in score as built above 
                    c.Start();                  // When compiling from strings, this call needed before performing
                    while (!c.PerformKsmps()) ;
                    c.Stop();
                }
                catch (Csound6NetException e)
                {
                    System.Console.WriteLine(string.Format("Csound failed to complete: {0}", e.Message));
                }
            }
        }

    }
}
