module AoC2025.Lib.Day11

open System

type PathCounts = {
    All: int64
    ThroughDac: int64
    ThroughFft: int64
    Valid: int64
}

let parseDevice (input: string) =
    let deviceAndOutputs = input.Split ':'
    let outputs = deviceAndOutputs[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
    (deviceAndOutputs[0], outputs |> List.ofArray)

let parseDevices (input: string list) =
    input
    |> List.map parseDevice
    |> Map.ofList

let rec countPathsToOut (devices: Map<string, string list>) (device: string) =
    devices[device]
    |> List.sumBy (fun output ->
        match output with
        | "out" -> 1
        | _ -> countPathsToOut devices output
    )

let rec countValidPathsToOut (devices: Map<string, string list>) (pathCounts: Map<string, PathCounts>) (device: string) =
    if pathCounts.ContainsKey device then
        pathCounts
    else
        let finalPathCounts =
            devices[device]
            |> List.fold (fun oldPathCounts output ->
                let newPathCounts = countValidPathsToOut devices oldPathCounts output
                let outputPathCounts = newPathCounts[output]
                newPathCounts
                |> Map.change device (fun result ->
                    match result with
                    | None -> Some outputPathCounts
                    | Some counts ->
                        Some {
                            All = counts.All + outputPathCounts.All
                            ThroughDac = counts.ThroughDac + outputPathCounts.ThroughDac
                            ThroughFft = counts.ThroughFft + outputPathCounts.ThroughFft
                            Valid = counts.Valid + outputPathCounts.Valid
                        }
                )
            ) pathCounts
        let devicePathCounts = finalPathCounts[device]
        let newDevicePathCounts =
            match device with
            | "dac" -> {devicePathCounts with
                            ThroughDac = devicePathCounts.All
                            Valid = devicePathCounts.ThroughFft
                       }
            | "fft" -> {devicePathCounts with
                            ThroughFft = devicePathCounts.All
                            Valid = devicePathCounts.ThroughDac
                       }
            | _ -> devicePathCounts
        finalPathCounts.Add(device, newDevicePathCounts)

let countPathsFromYouToOut (input: string list) =
    let devices = input |> parseDevices
    countPathsToOut devices "you"

let countValidPathsFromSvrToOut (input: string list) =
    let devices = input |> parseDevices
    let initialCounts = Map.empty.Add("out", { All = 1; ThroughDac = 0; ThroughFft = 0; Valid = 0 })
    let finalCounts = countValidPathsToOut devices initialCounts "svr"
    finalCounts["svr"].Valid
