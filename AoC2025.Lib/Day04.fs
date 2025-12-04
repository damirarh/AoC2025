module AoC2025.Lib.Day04

let parseMap (input: string list) =
    input
    |> List.map (fun row -> row.ToCharArray() |> Array.map (fun char -> char = '@') )
    |> List.toArray

let isRoll (map: bool array array) (col: int, row: int) =
    if col < 0 || col >= map[0].Length || row < 0 || row >= map.Length then
        false
    else
        map[row][col]

let isRollAccessible (map: bool array array) (col: int, row: int) =
    let adjacentRolls =
        seq {
            (col - 1, row - 1)
            (col, row - 1)
            (col + 1, row - 1)
            (col - 1, row)
            (col + 1, row)
            (col - 1, row + 1)
            (col, row + 1)
            (col + 1, row + 1)
        }
        |> Seq.filter (isRoll map)
        |> Seq.length
    adjacentRolls < 4

let tryRemoveRoll map coords =
    isRoll map coords && not (isRollAccessible map coords)

let countRolls (map: bool array array) =
    Seq.allPairs (seq { 0 .. map[0].Length - 1 }) (seq { 0 .. map.Length - 1 })
    |> Seq.filter (isRoll map)
    |> Seq.length

let rec removeRollsFromGrid (map: bool array array) =
    let newMap =
        [| 0 .. map.Length - 1 |]
        |> Array.map (fun row ->
            [| 0 .. map[row].Length - 1 |]
            |> Array.map (fun col -> tryRemoveRoll map (col, row)))

    let rollsRemoved = countRolls map - countRolls newMap
    if rollsRemoved = 0 then
        0
    else
        rollsRemoved + removeRollsFromGrid newMap

let countAccessibleRolls input=
    let map = parseMap input
    Seq.allPairs (seq { 0 .. map[0].Length - 1 }) (seq { 0 .. map.Length - 1 })
    |> Seq.filter (isRoll map)
    |> Seq.filter (isRollAccessible map)
    |> Seq.length

let countRemovableRolls input =
    input
    |> parseMap
    |> removeRollsFromGrid