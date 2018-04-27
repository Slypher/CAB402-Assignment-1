using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace CSharpSegmenter {

    interface Segment {

        Segment ParentSeg { get; set; }

    }

    class Pixel : Segment {

        private Segment parent;
        public Segment ParentSeg {
            get { return parent; }
            set { parent = value; }
        }

        public Tuple<int, int> Coordinate;
        public float[] Colour;

        public Pixel(Tuple<int, int> coordinate, byte[] colour) {
            this.Coordinate = coordinate;

            this.Colour = new float[colour.Length];

            for (int i = 0; i < colour.Length; i++)
                this.Colour[i] = colour[i];
        }

    }

    class Parent : Segment {

        private Segment parent;
        public Segment ParentSeg {
            get { return parent; }
            set { parent = value; }
        }

        public Segment Seg1;
        public Segment Seg2;

        public Parent(Segment seg1, Segment seg2) {
            this.Seg1 = seg1;
            this.Seg2 = seg2;

            seg1.ParentSeg = this;
            seg2.ParentSeg = this;
        }

    }


    class SegmentModule {

        public static int SegmentSize (Segment segment) {

            if (segment == null) return 0;
            else if (segment is Pixel) return 1;
            else {

                Parent seg = (Parent)segment;

                return SegmentSize(seg.Seg1) + SegmentSize(seg.Seg2);

            }

        }

        public static float[][] Transpose(float[][] segColours) {

            float[][] result = new float[segColours[0].Length][];

            for (int i = 0; i < segColours[0].Length; i++) result[i] = new float[segColours.Length];

            for (int i = 0; i < segColours.Length; i++)
                for (int j = 0; j < segColours[i].Length; j++)
                    result[j][i] = segColours[i][j];

            return result;

        }

        public static float[][] Desegment (Segment segment) {

            if (segment is Pixel) {

                float[][] result = { ((Pixel)segment).Colour };

                return result;

            } else {

                Parent seg = (Parent)segment;

                float[][] seg1Result = Desegment(seg.Seg1);
                float[][] seg2Result = Desegment(seg.Seg2);

                float[][] result = new float[seg1Result.Length + seg2Result.Length][];

                for (int i = 0; i < result.Length; i++) {

                    if (i <= seg1Result.Length - 1) result[i] = seg1Result[i];
                    else result[i] = seg2Result[i - seg1Result.Length];

                }

                return result;

            }

        }

        public static float[] Stddev (Segment segment) {

            if (segment is Pixel) {

                Pixel seg = (Pixel)segment;

                float[] result = new float[seg.Colour.Length];

                for (int i = 0; i < result.Length; i++) result[i] = 0F;

                return result;

            } else {

                float[][] colourBands = Transpose(Desegment(segment));
                float[] result = new float[colourBands.Length];

                for (int i = 0; i < colourBands.Length; i++) {
                    float len = colourBands[i].Length;
                    float avg = colourBands[i].Average();
                    float sum = 0F;

                    for (int j = 0; j < colourBands[i].Length; j++)
                        sum += (float)Math.Pow(colourBands[i][j] - avg, 2);

                    result[i] = (float)Math.Sqrt(sum / len);

                }

                return result;

            }

        }


        public static float MergeCost (Segment seg1, Segment seg2) {

            Parent combined = new Parent(seg1, seg2);

            int combSize = SegmentSize(combined);
            int seg1Size = SegmentSize(seg1);
            int seg2Size = SegmentSize(seg2);

            float[] combStddev = Stddev(combined);
            float[] seg1Stddev = Stddev(seg1);
            float[] seg2Stddev = Stddev(seg2);

            float sum = 0F;

            for (int i = 0; i < combStddev.Length; i++)
                sum += (combSize * combStddev[i]) - ((seg1Size * seg1Stddev[i]) + (seg2Size * seg2Stddev[i]));

            return sum;

        }

    }
}
