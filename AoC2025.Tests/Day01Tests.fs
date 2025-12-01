namespace AoC2025.Tests

open AoC2025.Lib.Day01
open NUnit.Framework
open Swensen.Unquote

type Day01Tests() =
    
    [<Test>]
    member this.``count stops at zero``() =
        test <@ (countStopsAtZero
                     [ "L68"
                       "L30"
                       "R48"
                       "L5"
                       "R60"
                       "L55"
                       "L1"
                       "L99"
                       "R14"
                       "L82" ]) = 3 @>

    [<Test>]
    member this.``count passes of zero``() =
        test <@ (countPassesOfZero
                     [ "L68"
                       "L30"
                       "R48"
                       "L5"
                       "R60"
                       "L55"
                       "L1"
                       "L99"
                       "R14"
                       "L82" ]) = 6 @>
