module AoC2025.Lib.Day09

type Coords = {
    X: int
    Y: int
}

type Line = {
    Coord: int
    Start: int
    End: int
}

type Lines = {
    Horizontal: Line list
    Vertical: Line list
}

let parseCoords (input: string) =
    let coords = input.Split(',') |> Array.map int
    { X = coords[0]; Y = coords[1] }

let parseInput (input: string list) =
    input |> List.map parseCoords |> List.toArray

let calculateArea (a: Coords) (b: Coords) =
    int64 ((abs (a.X - b.X)) + 1) * int64 ((abs (a.Y - b.Y)) + 1) 

let addLine (lines: Lines) (pair: Coords * Coords) =
    let coords1, coords2 = pair
    if coords1.X = coords2.X then
        let verticalLine = {
            Coord = coords1.X
            Start = min coords1.Y coords2.Y
            End = max coords1.Y coords2.Y
        }
        { lines with Vertical = verticalLine :: lines.Vertical }
    else
        let horizontalLine = {
            Coord = coords1.Y
            Start = min coords1.X coords2.X
            End = max coords1.X coords2.X
        }
        { lines with Horizontal = horizontalLine :: lines.Horizontal }

let getLines (redTiles: Coords array) =
    redTiles
    |> Array.pairwise
    |> Array.append [| (redTiles[redTiles.Length - 1], redTiles[0]) |]
    |> Array.fold addLine { Horizontal = List.empty; Vertical = List.empty } 

let rectangleIntersectsWithHorizontalLine (angle1: Coords) (angle2: Coords) (line: Line) =
    let minX = min angle1.X angle2.X
    let maxX = max angle1.X angle2.X
    let minY = min angle1.Y angle2.Y
    let maxY = max angle1.Y angle2.Y
    line.Coord > minY && line.Coord < maxY && line.Start <= maxX && line.End >= minX 

let rectangleIntersectsWithVerticalLine (angle1: Coords) (angle2: Coords) (line: Line) =
    let minX = min angle1.X angle2.X
    let maxX = max angle1.X angle2.X
    let minY = min angle1.Y angle2.Y
    let maxY = max angle1.Y angle2.Y
    line.Coord > minX && line.Coord < maxX && line.Start <= maxY && line.End >= minY

let rectangleIntersectsWithAnyLine (lines: Lines) (angle1: Coords) (angle2: Coords) =
    lines.Horizontal |> List.exists (rectangleIntersectsWithHorizontalLine angle1 angle2)
    ||
    lines.Vertical |> List.exists (rectangleIntersectsWithVerticalLine angle1 angle2)

let calculateLargestArea (input: string list) =
    let redTiles = input |> parseInput
    Seq.allPairs (seq { 0 .. redTiles.Length - 1 }) (seq { 0 .. redTiles.Length - 1 })
    |> Seq.filter (fun (a, b) -> a < b)
    |> Seq.map (fun (a, b) -> calculateArea redTiles[a] redTiles[b])
    |> Seq.max

let calculateLargestAreaWithRedAndGreenTilesOnly (input: string list) =
    let redTiles = input |> parseInput
    let lines = getLines redTiles
    Seq.allPairs (seq { 0 .. redTiles.Length - 1 }) (seq { 0 .. redTiles.Length - 1 })
    |> Seq.filter (fun (a, b) -> a < b)
    |> Seq.sortByDescending (fun (a, b) -> calculateArea redTiles[a] redTiles[b]) 
    |> Seq.find (fun (index1, index2) -> not (rectangleIntersectsWithAnyLine lines redTiles[index1] redTiles[index2]))
    |> (fun (a, b) -> calculateArea redTiles[a] redTiles[b])
