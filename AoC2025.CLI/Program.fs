module AoC2025.CLi.Program

open System.IO

printfn "Advent of Code 2025 Solutions"
printfn "============================="
printfn ""

let readAllLines path = File.ReadAllLines path |> Array.toList

let run title input func =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = func input
    let elapsed = stopWatch.ElapsedMilliseconds

    printfn $"{title} {result} ({elapsed} ms)"

printfn ""
printfn "Finished"