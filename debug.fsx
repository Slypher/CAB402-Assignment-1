

#I @"C:\Users\Connor\.nuget\packages\bitmiracle.libtiff.net\2.4.584.1"
#r @"lib\net40\BitMiracle.LibTiff.NET40.dll";;

#load "FSharpPureSegmenter\DitherModule.fs"
#load "FSharpPureSegmenter\TiffModule.fs"
#load "FSharpPureSegmenter\SegmentModule.fs"
#load "FSharpPureSegmenter\SegmentationModule.fs"
#load "FSharpPureSegmenter.UnitTests\TestData.fs"
#load "FSharpPureSegmenter.UnitTests\MockFunctions.fs"

open SegmentModule
open SegmentationModule
open TestData
open MockFunctions





let rec findRoot (segmentation: Segmentation) segment : Segment =
    match Map.tryFind segment segmentation with
    | None -> segment
    | Some parent ->
        if Map.containsKey parent segmentation then
            findRoot segmentation parent
        else parent
         
let insideBounds (N:int) (x:int, y:int) : bool =
    if x >= 0 && y >= 0 && x <= (pown 2 N) && y <= (pown N 2) then true
    else false

let getFourNeighbours (N:int) ((x:int, y:int)) : Set<Coordinate> =
    Set [(x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y)]
    |> Set.filter (insideBounds N)

let createNeighboursFunction (pixelMap:Coordinate->Segment) (N:int) : (Segmentation -> Segment -> Set<Segment>) =
    
    // Turn a parent segment into a list of coordinates
    let rec getCoords (segment:Segment) : List<Coordinate> =
        match segment with
        | Pixel (coord, _) -> [coord]
        | Parent (seg1, seg2) -> (getCoords seg1) @ (getCoords seg2)

    fun segmentation -> fun segment ->
        match segment with
        | Pixel (coord, _) ->
            getFourNeighbours N coord
            |> Set.map (fun neighbour -> pixelMap neighbour |> findRoot segmentation)
        | Parent (_, _) ->
            let coords = getCoords segment |> Set.ofList
            Seq.map (getFourNeighbours N) coords
            |> Set.unionMany
            |> Set.filter (fun coord -> Set.contains coord coords |> not)
            |> Set.map (fun neighbour -> pixelMap neighbour |> findRoot segmentation)

let createBestNeighbourFunction (neighbours:Segmentation->Segment->Set<Segment>) (threshold:float) : (Segmentation->Segment->Set<Segment>) =
    fun segmentation -> fun segment ->
        let mergers =
            neighbours segmentation segment
            |> Set.map (fun neighbour -> (mergeCost neighbour segment, neighbour))
            |> Set.toList

        if mergers.IsEmpty then Set.empty
        else
            let bestNeighbour = List.minBy fst mergers
            if fst bestNeighbour < threshold then Set[snd bestNeighbour]
            else Set.empty


let neighbours = (createNeighboursFunction dummyPixelMap 1)
let bestNeighbours = createBestNeighbourFunction dummyNeighbours 180.0

printf "%A" (getFourNeighbours 1 (0, 0))

printf "\n"
