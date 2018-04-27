using System;

namespace CSharpSegmenter {
    class Program {
        static void Main(string[] args) {

            var image = new TiffImage(args[0]);

            var N = 5;

            var threshold = 800.0F;

            var segmentation = SegmentationModule.Segment(image, N, threshold);

            image.overlaySegmentation("segmented.tif", N, segmentation);

            return; // We're all good in the hood
        }
    }
}
