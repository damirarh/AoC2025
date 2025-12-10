module AoC2025.Lib.Day10

open Flips
open Flips.Types

type Machine = {
    Lights: bool array
    Buttons: int array array
    Joltage: int array
}

let parseMachine (input: string) =
    let parts = input.Split ' '
    {
        Lights = parts[0].Trim('[',']').ToCharArray() |> Array.map (fun c -> c = '#')
        Buttons =
            parts
            |> Array.skip 1
            |> Array.take (parts.Length - 2)
            |> Array.map (fun wires -> wires.Trim('(',')').Split(',') |> Array.map int)
        Joltage = parts[parts.Length - 1].Trim('{','}').Split(',') |> Array.map int
    }

let rec getCombinations (items: int list) =
    if items.Length = 1 then
        [[]; [items.Head]]
    else
        let combinations = getCombinations items.Tail
        combinations
        |> List.append (combinations |> List.map (fun combination -> items.Head :: combination))
        |> List.sortBy _.Length

let toggleLights (lights: bool array) (indices: int array) =
    lights
    |> Array.mapi (fun index old -> old <> (indices |> Array.contains index)) 

let validateCombination (machine: Machine) (presses: int list) =
    presses
    |> List.map (fun index -> machine.Buttons[index])
    |> List.fold toggleLights (Array.replicate machine.Lights.Length false)
    |> Array.forall2 (fun a b -> a = b) machine.Lights

let getMinLightButtonPressCountForMachine (machine: Machine) =
    [ 0 .. machine.Buttons.Length - 1 ]
    |> getCombinations
    |> List.find (validateCombination machine)
    |> _.Length

let getTotalMinLightButtonPressCount (input: string list) =
    input
    |> List.map parseMachine
    |> List.sumBy getMinLightButtonPressCountForMachine

let getMinJoltageButtonPressCountForMachine (machine: Machine) =
    let maxIndividualJoltage = machine.Joltage |> Array.max

    let buttonPresses =
        machine.Buttons
        |> Array.mapi (fun index _ -> Decision.createInteger $"button {index}" 0 maxIndividualJoltage)

    let totalButtonPressesExpression = 
        buttonPresses
        |> Array.sumBy (fun press -> 1.0 * press)
        
    let totalButtonPressesObjective = Objective.create "minimum presses" Minimize totalButtonPressesExpression
    
    let buttonIndexesForJoltageIndex =
        [| 0 .. machine.Joltage.Length - 1 |]
        |> Array.map (fun joltageIndex ->
            [| 0 .. machine.Buttons.Length - 1 |]
            |> Array.filter (fun buttonIndex -> machine.Buttons[buttonIndex] |> Array.contains joltageIndex))
    
    let joltageConstraints =
        [| 0 .. machine.Joltage.Length - 1 |]
        |> Array.map (fun joltageIndex ->
            let buttonPressesExpression =
                buttonIndexesForJoltageIndex[joltageIndex]
                |> Array.sumBy (fun buttonIndex -> 1.0 * buttonPresses[buttonIndex])
            Constraint.create $"joltage {joltageIndex}" (buttonPressesExpression == float machine.Joltage[joltageIndex])
        )

    let model =
        Model.create totalButtonPressesObjective
        |> Model.addConstraints joltageConstraints
    
    let settings = {
        SolverType = SolverType.CBC
        MaxDuration = 10_000
        WriteLPFile = None
        WriteMPSFile = None
    }
    
    let result = Solver.solve settings model
    
    match result with
    | Optimal solution ->
        solution.DecisionResults.Values
        |> Seq.sum
        |> int
    | _ -> failwith $"%A{result}"

let getTotalMinJoltageButtonPressCount (input: string list) =
    input
    |> List.map parseMachine
    |> List.sumBy getMinJoltageButtonPressCountForMachine
