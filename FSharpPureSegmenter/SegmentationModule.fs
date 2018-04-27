module SegmentationModule

open SegmentModule
//open System.Net               
//open System.Xml.Linq              // Why are these here?

// Maps segments to their immediate parent segment that they are contained within (if any) 
type Segmentation = Map<Segment, Segment>



// Find the largest/top level segment that the given segment is a part of (based on the current segmentation)
let rec findRoot (segmentation: Segmentation) segment : Segment =
    match Map.tryFind segment segmentation with
    | None -> segment
    | Some parent ->
        if Map.containsKey parent segmentation then                         // If there's more of the tree to traverse...
            findRoot segmentation parent                                    // Then go traverse it
        else parent



// Initially, every pixel/coordinate in the image is a separate Segment
// Note: this is a higher order function which given an image, 
// returns a function which maps each coordinate to its corresponding (initial) Segment (of kind Pixel)
let createPixelMap (image:TiffModule.Image) : (Coordinate -> Segment) =
    fun coordinate -> Pixel (coordinate, TiffModule.getColourBands image coordinate)



// HELPER
// Determine if a coordinate is outside of an image with size 2^N x 2^N
let insideBounds (N:int) (x:int, y:int) : bool =
    if x >= 0 && y >= 0 && x < (pown 2 N) && y < (pown 2 N) then true
    else false
 
 // HELPER
 // Create four (or fewer) neighbours that are inside the bounds of an image, given a single coordinate
let getFourNeighbours (N:int) ((x:int, y:int)) : Set<Coordinate> =
    Set [(x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y)]
    |> Set.filter (insideBounds N)

// Find the neighbouring segments of the given segment (assuming we are only segmenting the top corner of the image of size 2^N x 2^N)
// Note: this is a higher order function which given a pixelMap function and a size N, 
// returns a function which given a current segmentation, returns the set of Segments which are neighbours of a given segment
let createNeighboursFunction (pixelMap:Coordinate->Segment) (N:int) : (Segmentation -> Segment -> Set<Segment>) =
    
    // Turn a parent segment into a list of coordinates
    let rec getCoords (segment:Segment) : List<Coordinate> =
        match segment with
        | Pixel (coord, _) -> [coord]
        | Parent (seg1, seg2) -> (getCoords seg1) @ (getCoords seg2)

    fun segmentation -> fun segment ->
        match segment with
        | Pixel (coord, _) ->
            getFourNeighbours N coord                                                   // For one pixel, create four neighbours...
            |> Set.map (fun neighbour -> pixelMap neighbour |> findRoot segmentation)   // Get the segment and then find the root
        | Parent (_, _) ->
            let coords = getCoords segment |> Set.ofList                                // Get the coordinates for every pixel in the segment...
            Seq.map (getFourNeighbours N) coords                                        // For each coord, createFourNeighbours
            |> Set.unionMany
            |> Set.filter (fun coord -> Set.contains coord coords |> not)               // Now trim the set of our input pixels
            |> Set.map (fun neighbour -> pixelMap neighbour |> findRoot segmentation)   // Get the segment and then find the root



 // Find the neighbour(s) of the given segment that has the (equal) best merge cost
 // (exclude neighbours if their merge cost is greater than the threshold)
let createBestNeighbourFunction (neighbours:Segmentation->Segment->Set<Segment>) (threshold:float) : (Segmentation->Segment->Set<Segment>) =
    fun segmentation -> fun segment ->
        let bestNeighbours =
            neighbours segmentation segment                                             // Get the neighbours of this segment
            |> Set.map (fun neighbour -> (mergeCost neighbour segment, neighbour))      // Give each neighbour a merge cost
            |> Set.filter (fun neighbour -> fst neighbour <= threshold)                 // Remove neighbours outside of threshold
            |> Set.toList

        if bestNeighbours.IsEmpty then Set.empty
        else Set [List.minBy fst bestNeighbours |> snd]                                 // Return the segment associated with lowest merge cost



// Try to find a neighbouring segmentB such that:
//     1) segmentB is one of the best neighbours of segment A, and 
//     2) segmentA is one of the best neighbours of segment B
// if such a mutally optimal neighbour exists then merge them,
// otherwise, choose one of segmentA's best neighbours (if any) and try to grow it instead (gradient descent)
let createTryGrowOneSegmentFunction (bestNeighbours:Segmentation->Segment->Set<Segment>) (pixelMap:Coordinate->Segment) : (Segmentation->Coordinate->Segmentation) =

    // Recursively attempt to find and merge two mutual best neighbours
    let rec growOneSegment (segmentation:Segmentation) (bestNeighbours:Segmentation->Segment->Set<Segment>) (pixelMap:Coordinate->Segment) (segment:Segment) : Segmentation =
        match bestNeighbours segmentation segment |> Set.toList with
        | [] -> segmentation
        | _ as bestNeighbour ->
            let neighbour = bestNeighbour.[0]
            let neighboursNeighbour = bestNeighbours segmentation neighbour             // Makes you think 🤔

            if neighboursNeighbour.Contains segment then                                // If we are mutually best neighbours...
                let newSeg = Parent (segment, neighbour)                                // Then let's be friends!
                segmentation                                                            // Now add it to the segmentation
                |> Map.add neighbour newSeg
                |> Map.add segment newSeg
            else growOneSegment segmentation bestNeighbours pixelMap neighbour          // Next up: neighboursNeighboursNeighbour

    fun segmentation -> fun coordinate ->
        pixelMap coordinate
        |> findRoot segmentation                                                        // Get our segment...
        |> growOneSegment segmentation bestNeighbours pixelMap                          // Now grow it (recursively)



// Try to grow the segments corresponding to every pixel on the image in turn 
// (considering pixel coordinates in special dither order)
let createTryGrowAllCoordinatesFunction (tryGrowPixel:Segmentation->Coordinate->Segmentation) (N:int) : (Segmentation->Segmentation) =

    // Recursively grow the first coordinate in a sequence of coordinates
    let rec growAllCoordinates (tryGrowPixel:Segmentation->Coordinate->Segmentation) (coordinates:seq<Coordinate>) (segmentation:Segmentation) : Segmentation =
        if Seq.isEmpty coordinates then segmentation
        else
            Seq.head coordinates
            |> tryGrowPixel segmentation
            |> growAllCoordinates tryGrowPixel (Seq.tail coordinates)

    growAllCoordinates tryGrowPixel (DitherModule.coordinates N)



// Keep growing segments as above until no further merging is possible
let createGrowUntilNoChangeFunction (tryGrowAllCoordinates:Segmentation->Segmentation) : (Segmentation->Segmentation) =

    // Recursively grow the provided segmentation until there is no change detected
    let rec growSegmentation (tryGrowAllCoordinates:Segmentation->Segmentation) (segmentation:Segmentation) : Segmentation =
        match tryGrowAllCoordinates segmentation with
        | segmentation -> segmentation
        | _ as result -> growSegmentation tryGrowAllCoordinates result                  // Why does VS2017 complain about this line?

    growSegmentation tryGrowAllCoordinates



// Segment the given image based on the given merge cost threshold, but only for the top left corner of the image of size (2^N x 2^N)
let segment (image:TiffModule.Image) (N: int) (threshold:float)  : (Coordinate -> Segment) =
    let pixelMap = createPixelMap image
    let neighbours = createNeighboursFunction pixelMap N
    let bestNeighbours = createBestNeighbourFunction neighbours threshold
    let tryGrowOne = createTryGrowOneSegmentFunction bestNeighbours pixelMap
    let tryGrowAll = createTryGrowAllCoordinatesFunction tryGrowOne N                   // There didn't seem to be a way to use pipes here :(
    let growUntilNoChange = createGrowUntilNoChangeFunction tryGrowAll                  // Maybe if the function signatures were different...

    let segmentation = growUntilNoChange Map.empty

    pixelMap >> findRoot segmentation                                                   // I finally found a use for this operator!
