using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace csoundAPI_examples
{
    /*
     * Support class used in Exercise 7
     */
    public class RandomLine
    {
        static Random _random = new Random();

        public RandomLine(double _base, double _range)
        {
            CurVal = 0.0;
            Reset();
            Base = _base;
            Range = _range;
        }

        private double Base { get; set; }

        private double CurVal { get; set; }

        private int Dur { get; set; }

        private double End { get; set; }

        private double Increment { get; set; }

        private double Range { get; set; }


        private void Reset()
        {
            Dur = _random.Next(256, 512);
            End = _random.NextDouble();
            Increment = (End - CurVal) / Dur;
        }

        public double Value
        {
            get {
                Dur -= 1;
                if (Dur < 0) Reset();
                double retval = CurVal;
                CurVal += Increment;
                return Base + (Range * retval);
            }
        }
    }
}
