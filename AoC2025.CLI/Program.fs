module AoC2025.CLi.Program

open System.IO
open AoC2025.Lib

printfn "Advent of Code 2025 Solutions"
printfn "============================="
printfn ""

let readAllLines path = File.ReadAllLines path |> Array.toList
let readAllText path = File.ReadAllText path

let run title input func =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = func input
    let elapsed = stopWatch.ElapsedMilliseconds

    printfn $"{title} {result} ({elapsed} ms)"

run "Day 01 Part 1:" (readAllLines "Day01.txt") Day01.countStopsAtZero
run "Day 01 Part 2:" (readAllLines "Day01.txt") Day01.countPassesOfZero
run "Day 02 Part 1:" (readAllText "Day02.txt") Day02.calculateSumOfInvalidIdsRepeatedTwice
run "Day 02 Part 2:" (readAllText "Day02.txt") Day02.calculateSumOfInvalidIdsRepeatedMoreThanTwice
run "Day 03 Part 1:" (readAllLines "Day03.txt") (Day03.calculateTotalJoltage 2)
run "Day 03 Part 2:" (readAllLines "Day03.txt") (Day03.calculateTotalJoltage 12)
run "Day 04 Part 1:" (readAllLines "Day04.txt") Day04.countAccessibleRolls
run "Day 04 Part 2:" (readAllLines "Day04.txt") Day04.countRemovableRolls

printfn ""
printfn "Finished"