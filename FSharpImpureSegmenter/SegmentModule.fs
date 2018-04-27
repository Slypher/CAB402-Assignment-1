module SegmentModule

type Coordinate = (int * int) // x, y coordinate of a pixel
type Colour = byte list       // one entry for each colour band, typically: [red, green and blue]

type Segment = 
    | Pixel of Coordinate * Colour
    | Parent of Segment * Segment 



// HELPER
// Transpose a list of [r,g,b] values into a list of [ [r, ...], [g, ...], [b, ...] ]
let transpose (rows : list<list<'T>>) : list<list<'T>> =
    let n = List.length (List.head rows)
    List.init n (fun i -> (List.map (List.item i) rows))

// HELPER
// Flatten a segment into a list where each element is a segments colour
let rec desegment (segment:Segment) : list<list<float>> =
    match segment with
    | Pixel (_, colour) -> [List.map float colour]                              // The colour band of the pixel, as float values
    | Parent (seg01, seg02) -> (desegment seg01) @ (desegment seg02)            // Concatenate the two de-segmented segments

// return a list of the standard deviations of the pixel colours in the given segment
// the list contains one entry for each colour band, typically: [red, green and blue]
let stddev (segment: Segment) : float list =
    match segment with
    | Pixel (_, colour) -> List.map (fun _ -> 0.0) colour
    | _ ->
        desegment segment                                                       // First, flatten our segment tree into their colours ( Segment -> [ [r,g,b], ... ] )
        |> transpose                                                            // Now transpose into a list for each colour band ( [ [r,g,b], ... ] -> [ [r, ...], [g, ...], [b, ...] ] )
        |> List.map (fun colourN ->                                             // Now we can do the maths for each band...
            let len = List.length colourN
            let avg = List.average colourN
            List.map (fun colourValue -> pown (colourValue - avg) 2) colourN
            |> List.sum
            |> (*) (1.0 / float len)                                            // Really cool that operators are also functions
            |> sqrt)





// HELPER
// Calculate the number of pixels in a segment
let rec segmentSize (segment:Segment) : float =
    match segment with
    | Pixel (_, _) -> 1.0
    | Parent (seg1, seg2) -> (segmentSize seg1) + (segmentSize seg2)

// determine the cost of merging the given segments: 
// equal to the standard deviation of the combined the segments minus the sum of the standard deviations of the individual segments, 
// weighted by their respective sizes and summed over all colour bands
let mergeCost segment1 segment2 : float = 
    let combined = Parent (segment1, segment2)
    
    let combSize = segmentSize combined
    let seg1Size = segmentSize segment1
    let seg2Size = segmentSize segment2

    (stddev combined, stddev segment1, stddev segment2)                         // Calculate our standard deviations...
    |||> List.map3 (fun combStddevN seg1StddevN seg2StddevN ->                  // Now do the maths to calculate the cost for each band...
        combSize * combStddevN
        |> (+) ( - ((seg1StddevN * seg1Size) + (seg2StddevN * seg2Size))))
    |> List.sum
