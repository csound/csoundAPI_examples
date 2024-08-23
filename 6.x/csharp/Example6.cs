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
        /* Example 6 - Generating a Score
         * 
         * This example continues on from Example 5, rewriting the example using
         * a Class called Note (in another class file for better reuse).
         * The note example has its ToString() method implemented
         * to generate a well-formatted Csound SCO note.  
         *
         * This example also shows how a list of notes could be used multiple times.
         * The first loop through we use the notes as-is, and during the second time
         * we generate the notes again with the same properties except we alter the 
         * fifth p-field up 4 semitones. 
         *
         * Note: Altering a Notes values like this is alright for this example, but 
         * it is a destructive edit.  Real world code might make copies of Notes or 
         * alter the score generation to maintain the original values. 
         */
        public void Example6()
        {
            var r = new Random();
            //Create score from a generated list of Note objects using Midi pitches 60-75.
            var notes = new List<Note>();
            for (int i = 0; i < 13; i++)       // P1  P2-start P3-Dur P4-amp   P5-frq 
                notes.Add(new Note(new double[] { 1, i * .25, .25,    .5,   r.Next(60, 75) }));

            //Capture the note values as Csound parameters in a .net System.Text.StringBuilder
            var sco = new StringBuilder();
            foreach (var n in notes)
                sco.AppendLine(n.ToString()); //note.ToString() will look like a score statement

            //Add a major third and time displacement to the Note objects and put them in the score as well.
            foreach (var n in notes)
            {
                if (n.Pfields.Length > 4) n.Pfields[4] += 4; //pitch (p5)
                if (n.Pfields.Length > 1) n.Pfields[1] += .125; //start time (p2)
                sco.AppendLine(n.ToString()); //More score statements with new freq and start
            }


            using (var c = new Csound6Net())
            {
                c.SetOption("-odac");   // Set option for Csound
                try
                {
                    //Now that the score has been generated, apply it to Csound:
                    c.CompileOrc(orc2);         //Compile the matching orchestra
                    c.ReadScore(sco.ToString());//and apply the generated score

                    c.Start();
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
