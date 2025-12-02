module AoC2025.Lib.Day02

type Range = {
    First: string
    Last: string
}

let parseRange (input: string) =
    let pair = input.Split '-'
    { First = pair[0]; Last = pair[1] }

let parseInput (line: string) =
    line.Split ','
    |> Array.map parseRange 

let adjustRangeLength noParts range =
    let firstLengthOk = String.length range.First % noParts = 0
    let lastLengthOk = String.length range.Last % noParts = 0

    if firstLengthOk && lastLengthOk then
        Some range
    else if not firstLengthOk && not lastLengthOk then
        None
    else if lastLengthOk then
        Some({ range with First = Array.create range.First.Length "0" |> Array.append [| "1" |] |> String.concat "" })
    else
        Some({ range with Last = Array.create range.First.Length "9" |> String.concat "" })

let repeat times number =
    let numberAsString = string number
    Seq.replicate times numberAsString
    |> String.concat ""
    |> int64    

let getInvalidIdsInRange noParts range =
    match adjustRangeLength noParts range with
    | None -> [||]
    | Some adjustedRange ->
        let firstValue = int64 adjustedRange.First
        let lastValue = int64 adjustedRange.Last
        
        let partLength = adjustedRange.First.Length / noParts
        let startValue = adjustedRange.First.Substring(0, partLength) |> int64 
        let endValue = adjustedRange.Last.Substring(0, partLength) |> int64

        seq {startValue .. endValue}
        |> Seq.map (repeat noParts)
        |> Seq.filter (fun id -> id >= firstValue && id <= lastValue)
        |> Seq.toArray

let getInvalidIdsWithAnyPartsInRange range =
    [| 2 .. range.Last.Length |]
    |> Array.collect (fun noParts -> getInvalidIdsInRange noParts range)
    |> Array.distinct

let calculateSumOfInvalidIdsRepeatedTwice input =
    input
    |> parseInput
    |> Array.collect (getInvalidIdsInRange 2)
    |> Array.sum

let calculateSumOfInvalidIdsRepeatedMoreThanTwice input =
    input
    |> parseInput
    |> Array.collect getInvalidIdsWithAnyPartsInRange
    |> Array.sum
