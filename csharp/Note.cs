using System;
using System.Text;

namespace csoundAPI_examples
{

    /**
     * Support class for exercise 6 to hold an array of score parameters for the orc2 instrument.
     */
    public class Note
    {
        public Note(double[] _parms)
        {
            Pfields = _parms;
        }

        public double[] Pfields { get; private set; }

        public override string ToString()
        {
            var note = new StringBuilder("i");
            note.Append((int)Pfields[0]);
            for (int i = 1; i < Pfields.Length; i++)
            {
                note.Append(' ');
                if (i != 4) note.Append(Pfields[i]);
                else
                {
                    note.Append(string.Format("{0:##}.{1:00}", (int)(3.0 + (Pfields[i] / 12.0)), Pfields[i] % 12));
                }

            }
            return note.ToString();
        }
    }


}
