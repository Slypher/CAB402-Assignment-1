using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace CSharpSegmenter {
    class SegmentationModule {

        public static Pixel[][] pixelMap;

        public static Segment FindRoot(Segment segment) {

            if (segment.ParentSeg != null) return FindRoot(segment.ParentSeg);
            else return segment;

        }

        public static Boolean InsideBounds(int N, Tuple<int, int> coord) {

            if (coord.Item1 >= 0 && coord.Item2 >= 0 && coord.Item1 < Math.Pow(2, N) && coord.Item2 < Math.Pow(2, N)) return true;
            else return false;

        }

        public static void CreatePixelMap(TiffImage image, float N) {

            pixelMap = new Pixel[(int)Math.Pow(2, N)][];

            for (int i = 0; i < Math.Pow(2, N); i++) {

                pixelMap[i] = new Pixel[(int)Math.Pow(2, N)];

                for (int j = 0; j < Math.Pow(2, N); j++)
                    pixelMap[i][j] = new Pixel(Tuple.Create(i, j), image.getColourBands(i, j));

            }

        }

        public static Tuple<int, int>[] GetFourNeighbours(int N, Tuple<int, int> coord) {

            List<Tuple<int, int>> result = new List<Tuple<int, int>>();

            var top = Tuple.Create(coord.Item1, coord.Item2 - 1);
            var right = Tuple.Create(coord.Item1 + 1, coord.Item2);
            var bottom = Tuple.Create(coord.Item1, coord.Item2 + 1);
            var left = Tuple.Create(coord.Item1 - 1, coord.Item2);

            if (InsideBounds(N, top)) result.Add(top);
            if (InsideBounds(N, right)) result.Add(right);
            if (InsideBounds(N, bottom)) result.Add(bottom);
            if (InsideBounds(N, left)) result.Add(left);
            
            return result.ToArray();

        }

        public static Tuple<int, int>[] GetCoords(Segment segment) {

            if (segment is Pixel) {

                Tuple<int, int>[] result = { ((Pixel)segment).Coordinate };

                return result;

            } else {

                Parent seg = (Parent)segment;

                Tuple<int, int>[] result = new Tuple<int, int>[SegmentModule.SegmentSize(segment)];

                Tuple<int, int>[] seg1Result = GetCoords(seg.Seg1);
                Tuple<int, int>[] seg2Result = GetCoords(seg.Seg2);

                for (int i = 0; i < result.Length; i++) {

                    if (i < seg1Result.Length) result[i] = seg1Result[i];
                    else result[i] = seg2Result[i - seg1Result.Length];

                }

                return result;

            }

        }

        public static Boolean ValidNeighbour (Tuple<int, int> neighbour, Parent segment, List<Tuple<int, int>> neighbours) {

            if (neighbours.Contains(neighbour)) return false;

            if (Array.IndexOf(GetCoords(segment), neighbour) > -1) return false;

            return true;

        }

        public static Segment[] CreateNeighbours(Segment segment, int N) {

            if (segment is Pixel) {

                Pixel seg = (Pixel)segment;

                var neighbours = GetFourNeighbours(N, seg.Coordinate);

                Segment[] result = new Segment[neighbours.Length];

                for (int i = 0; i < neighbours.Length; i++) {

                    result[i] = FindRoot(pixelMap[neighbours[i].Item1][neighbours[i].Item2]);

                }

                return result;

            } else {

                Parent seg = (Parent)segment;

                List<Tuple<int, int>> neighbourCoords = new List<Tuple<int, int>>();

                var coords = GetCoords(seg);

                for (int i = 0; i < coords.Length; i++) {

                    var neighbours = GetFourNeighbours(N, coords[i]);

                    for (int j = 0; j < neighbours.Length; j++) 
                        if (ValidNeighbour(neighbours[j], seg, neighbourCoords))
                            neighbourCoords.Add(neighbours[j]);

                }

                Segment[] result = new Segment[neighbourCoords.Count];

                for (int i = 0; i < neighbourCoords.Count; i++)
                    result[i] = FindRoot(pixelMap[neighbourCoords[i].Item1][neighbourCoords[i].Item2]);

                return result;

            }

        }

        public static Segment CreateBestNeighbour (Segment segment, float threshold, int N) {

            var neighbours = CreateNeighbours(segment, N);

            Tuple<float, Segment>[] mergers = new Tuple<float, Segment>[neighbours.Length];

            float minCost = threshold;
            Segment minSeg = null;

            for (int i = 0; i < neighbours.Length; i++)
                if (SegmentModule.MergeCost(neighbours[i], segment) < minCost) minSeg = neighbours[i];

            return minSeg;

        }

        public static Boolean TryGrowOneSegment (Tuple<int, int> coordinate, float threshold, int N) {

            Segment self = FindRoot(pixelMap[coordinate.Item1][coordinate.Item2]);
            Segment bestNeighbour = CreateBestNeighbour(self, threshold, N);

            if (bestNeighbour == null) return false;

            Segment neighboursNeighbour = CreateBestNeighbour(bestNeighbour, threshold, N);

            if (neighboursNeighbour == null) return false;

            if (GetCoords(self).SequenceEqual(GetCoords(neighboursNeighbour))) {

                Parent parent = new Parent(self, bestNeighbour);

                return true;

            }

            return false;

        }

        public static Boolean TryGrowAllCoordinates (float threshold, int N) {

            var coordinates = Dither.coordinates(N);

            foreach (Tuple<int, int> coord in coordinates)
                if (TryGrowOneSegment(coord, threshold, N)) return true;

            return false;

        }

        public static void GrowUntilNoChange (float threshold, int N) {

            while (TryGrowAllCoordinates(threshold, N));

        }

        public static SegmentationCoord Segment(TiffImage image, int N, float threshold) {

            CreatePixelMap(image, N);
            GrowUntilNoChange(threshold, N);

            return (x, y) => FindRoot(pixelMap[x][y]);
        }
    }
}
