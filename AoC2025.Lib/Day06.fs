module AoC2025.Lib.Day06

open System

type Problem = {
    Operation: int64 -> int64 -> int64
    Operands: int64 list
}

let parseOperation operator =
    match operator with
    | "+" -> fun a b -> a + b
    | "*" -> fun a b -> a * b
    | _ -> failwith "Operand parsing failed"

let parseProblem (column: string list) =
    match (column |> List.rev) with
    | operator :: operands ->
        {
            Operation = parseOperation operator
            Operands = operands |> List.map int64
        }
    | _ -> failwith "Column parsing failed" 
    

let parseInput (input: string list) =
    input
    |> List.map _.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> List.map Array.toList
    |> List.transpose
    |> List.map parseProblem

let parseOperandGroup (lines: string array) =
    lines
    |> Array.toList
    |> List.map int64

let rec parseOperands (operands: int64 list list) (lines: string array) =
    match lines |> Array.tryFindIndex (fun line -> line.Trim().Length = 0) with
    | None -> parseOperandGroup lines :: operands
    | Some index ->
        let group = parseOperandGroup (lines |> Array.take index)
        let remainingOperands = lines |> Array.skip (index + 1)
        parseOperands (group :: operands) remainingOperands

let parseInputRightToLeft (input: string list) =
    let maxLineLength = input |> List.map _.Length |> List.max 
    let array = input |> List.toArray
    let operations =
        array[array.Length - 1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map parseOperation
        |> Array.toList
        |> List.rev
    let operands =
        array
        |> Array.take (array.Length - 1)
        |> Array.map _.PadRight(maxLineLength)
        |> Array.map _.ToCharArray()
        |> Array.transpose
        |> Array.map (fun chars -> chars |> String)
        |> parseOperands []
    List.zip operations operands
    |> List.map (fun (operation, operands) -> { Operation = operation; Operands = operands })

let solveProblem problem =
    problem.Operands
    |> List.reduce problem.Operation

let calculateGrandTotal (input: string list) =
    input
    |> parseInput
    |> List.sumBy solveProblem

let calculateGrandTotalRightToLeft (input: string list) =
    input
    |> parseInputRightToLeft
    |> List.sumBy solveProblem