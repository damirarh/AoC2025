module AoC2025.Lib.Day07

type State = {
    SplitCount: int
    Beams: Map<int, int64>
}

let addBeam (count: int64) (prevValue: int64 option) =
    match prevValue with
    | None -> Some count
    | Some prevCount -> Some (prevCount + count) 

let processBeam (line: string) (state: State) (index: int) (count: int64) =
    match line[index] with
    | '^' ->
        {
            SplitCount = state.SplitCount + 1
            Beams =
                state.Beams
                |> Map.change (index - 1) (addBeam count)
                |> Map.change (index + 1) (addBeam count)
        }
    | _ -> { state with Beams = state.Beams |> Map.change index (addBeam count) }

let processBeams (state: State) (line: string) =
    let startingState = {
        SplitCount = state.SplitCount
        Beams = Map.empty
    }
    state.Beams
    |> Map.fold (processBeam line) startingState

let simulate (input: string list) =
    let startingState = {
        SplitCount = 0
        Beams = Map [(input.Head.IndexOf('S'), 1)]
    }
    input
    |> List.skip 1
    |> List.fold processBeams startingState

let countBeamSplits (input: string list) =
    (simulate input).SplitCount
    
let countTimelines (input: string list) =
    (simulate input).Beams.Values |> Seq.sum