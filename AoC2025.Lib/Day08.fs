module AoC2025.Lib.Day08

open System

type Junction = {
    Coords: int64 array
    Circuit: int
}

type Grid = {
    Junctions: Map<int, Junction>
    Distances: (int * int) list
    Circuits: Map<int, Set<int>>
    LastConnection: (int * int) option
}

let parseInput (input: string list) =
    input
    |> List.mapi (fun index coords -> (index, {
        Coords = (coords.Split(',') |> Array.map int64)
        Circuit = index
    }))
    |> Map.ofList

let calculateDistance (a: int64 array) (b: int64 array) =
    Array.zip a b
    |> Array.sumBy (fun (a, b) -> (a - b) * (a - b))
    |> float
    |> Math.Sqrt

let rec calculateDistances (junctions: Map<int, Junction>) =
    Seq.allPairs (seq { 0 .. junctions.Count - 1 }) (seq { 0 .. junctions.Count - 1 })
    |> Seq.filter (fun (a, b) -> a < b)
    |> Seq.map (fun (a, b) -> ((a, b), calculateDistance junctions[a].Coords junctions[b].Coords))
    |> Seq.sortBy snd
    |> Seq.map fst
    |> Seq.toList

let makeShortestConnection (grid: Grid) =
    let firstJunctionIndex, secondJunctionIndex = grid.Distances.Head
    let firstJunction = grid.Junctions[firstJunctionIndex]
    let secondJunction = grid.Junctions[secondJunctionIndex]
    if firstJunction.Circuit = secondJunction.Circuit then
        { grid with Distances = grid.Distances.Tail }
    else
        let firstCircuit = grid.Circuits[firstJunction.Circuit]
        let secondCircuit = grid.Circuits[secondJunction.Circuit]
        { grid with
            Distances = grid.Distances.Tail
            Circuits = grid.Circuits
                |> Map.remove secondJunction.Circuit
                |> Map.add firstJunction.Circuit (firstCircuit |> Set.union secondCircuit)
            Junctions = secondCircuit
                |> Set.fold (fun junctions junctionIndex ->
                    junctions.Change(junctionIndex, fun old ->
                        Some { old.Value with Circuit = firstJunction.Circuit })) grid.Junctions
            LastConnection = Some (firstJunctionIndex, secondJunctionIndex)
        }

let rec connectNJunctions (count: int) (grid: Grid) =
    if count = 0 then
        grid
    else
        connectNJunctions (count - 1) (makeShortestConnection grid)

let rec connectAllJunctions (grid: Grid) =
    if grid.Circuits.Count = 1 then
        grid
    else
        connectAllJunctions (makeShortestConnection grid)

let createInitialGrid (input: string list) =
    let junctions = parseInput input
    {
        Junctions = junctions
        Distances = junctions |> calculateDistances
        Circuits =
            seq { 0 .. junctions.Count - 1 }
            |> Seq.map (fun index -> (index, Set(seq { index })))
            |> Map.ofSeq
        LastConnection = None
    }

let multiplySizesOfLargestCircuits (count: int) (input: string list)=
    input
    |> createInitialGrid
    |> connectNJunctions count
    |> _.Circuits.Values
    |> Seq.map _.Count
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (fun a b -> a * b) 

let multiplyXCoordsOfLastJunctions (input: string list)=
    let finalGrid =
        input
        |> createInitialGrid
        |> connectAllJunctions
    
    finalGrid.Junctions[fst finalGrid.LastConnection.Value].Coords[0] * finalGrid.Junctions[snd finalGrid.LastConnection.Value].Coords[0] 
