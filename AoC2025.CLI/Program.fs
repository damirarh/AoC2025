module AoC2025.CLi.Program

open System.IO
open AoC2025.Lib

printfn "Advent of Code 2025 Solutions"
printfn "============================="
printfn ""

let readAllLines path = File.ReadAllLines path |> Array.toList

let run title input func =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = func input
    let elapsed = stopWatch.ElapsedMilliseconds

    printfn $"{title} {result} ({elapsed} ms)"

run "Day 01 Part 1:" (readAllLines "Day01.txt") Day01.countStopsAtZero
run "Day 01 Part 2:" (readAllLines "Day01.txt") Day01.countPassesOfZero

printfn ""
printfn "Finished"