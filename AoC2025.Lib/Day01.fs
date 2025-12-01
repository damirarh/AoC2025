module AoC2025.Lib.Day01

open System.Text.RegularExpressions

type Direction =
    | Left
    | Right

type Rotation = {
    Direction: Direction
    Distance: int
}

type State = {
    Position: int
    ZeroStopCount: int
    ZeroPassCount: int
}

let parseInput input =
    let result = Regex.Match(input, @"(.)(\d+)")
    
    match result.Success with
    | true -> {
            Direction = match result.Groups[1].Value with
                        | "L" -> Left
                        | "R" -> Right
                        | _ -> failwith "Direction parsing failed"
            Distance = int result.Groups[2].Value
        }
    | false -> failwith "Input parsing failed"

let wrapToDial value =
    (value % 100 + 100) % 100 

let rotate state rotation =
    let sign = match rotation.Direction with
               | Left -> -1
               | Right -> 1
    let newPosition = wrapToDial (state.Position + sign * rotation.Distance)
    {
        Position = newPosition
        ZeroStopCount = match newPosition with
                        | 0 -> state.ZeroStopCount + 1
                        | _ -> state.ZeroStopCount
        ZeroPassCount = state.ZeroPassCount
                        +
                        rotation.Distance / 100
                        +
                        match rotation.Direction with
                        | Left -> if newPosition = 0 || state.Position > 0 && newPosition > state.Position then 1 else 0
                        | Right -> if newPosition < state.Position then 1 else 0
    }
    

let countStopsAtZero input =
    let startingState = { Position = 50; ZeroStopCount = 0; ZeroPassCount = 0 }
    let finalState = input
                     |> List.map parseInput
                     |> List.fold rotate startingState
    finalState.ZeroStopCount

let countPassesOfZero input =
    let startingState = { Position = 50; ZeroStopCount = 0; ZeroPassCount = 0 }
    let finalState = input
                     |> List.map parseInput
                     |> List.fold rotate startingState
    finalState.ZeroPassCount
