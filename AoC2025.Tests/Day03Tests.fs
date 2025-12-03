namespace AoC2025.Tests

open AoC2025.Lib.Day03
open NUnit.Framework
open Swensen.Unquote

type Day03Tests() =
    
    [<Test>]
    member this.``calculate total joltage with 2 betteries``() =
        test <@ (calculateTotalJoltage 2
                    [ "987654321111111"
                      "811111111111119"
                      "234234234234278"
                      "818181911112111" ]) = 357 @>
    
    [<Test>]
    member this.``calculate new joltage with 12 batteries``() =
        test <@ (calculateTotalJoltage 12
                    [ "987654321111111"
                      "811111111111119"
                      "234234234234278"
                      "818181911112111" ]) = 3121910778619L @>
