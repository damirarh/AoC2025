namespace AoC2025.Tests

open AoC2025.Lib.Day04
open NUnit.Framework
open Swensen.Unquote

type Day04Tests() =
    
    [<Test>]
    member this.``count accessible rolls``() =
        test <@ (countAccessibleRolls
                    [ "..@@.@@@@."
                      "@@@.@.@.@@"
                      "@@@@@.@.@@"
                      "@.@@@@..@."
                      "@@.@@@@.@@"
                      ".@@@@@@@.@"
                      ".@.@.@.@@@"
                      "@.@@@.@@@@"
                      ".@@@@@@@@."
                      "@.@.@@@.@." ]) = 13 @>
    
    [<Test>]
    member this.``count removable rolls``() =
        test <@ (countRemovableRolls
                    [ "..@@.@@@@."
                      "@@@.@.@.@@"
                      "@@@@@.@.@@"
                      "@.@@@@..@."
                      "@@.@@@@.@@"
                      ".@@@@@@@.@"
                      ".@.@.@.@@@"
                      "@.@@@.@@@@"
                      ".@@@@@@@@."
                      "@.@.@@@.@." ]) = 43 @>
