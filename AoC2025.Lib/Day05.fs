module AoC2025.Lib.Day05

open System.Text.RegularExpressions

type Range = {
    From: int64
    To: int64
}

let parseRange line =
    let result = Regex.Match(line, @"(\d+)-(\d+)")
    match result.Success with
    | true -> {
            From = int64 result.Groups[1].Value
            To = int64 result.Groups[2].Value
        }
    | false -> failwith "Range parsing failed"

let isNonEmptyLine (line: string) =
    line.Length > 0

let parseInput(input: string list) =
    let ranges =
        input
        |> List.takeWhile isNonEmptyLine
        |> List.map parseRange
        
    let ingredients =
        input
        |> List.rev
        |> List.takeWhile isNonEmptyLine
        |> List.map int64

    (ranges, ingredients)

let isInRange ingredient range =
    ingredient >= range.From && ingredient <= range.To

let isValidIngredient validRanges ingredient =
    validRanges
    |> List.exists (isInRange ingredient)

let tryMergeRange ranges range =
    match ranges with
    | head :: tail when head.To >= range.From ->
        { head with To = max head.To range.To } :: tail
    | ranges -> range :: ranges

let countIngredientsInRange range =
    range.To - range.From + 1L

let countValidIngredients input =
    let validRanges, ingredients = input |> parseInput
    ingredients
    |> List.filter (isValidIngredient validRanges)
    |> List.length

let countValidIngredientIds input =
    let validRanges, _ = input |> parseInput
    validRanges
    |> List.sortBy (fun range -> range.From)
    |> List.fold tryMergeRange []
    |> List.sumBy countIngredientsInRange