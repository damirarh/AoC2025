namespace AoC2025.Tests

open AoC2025.Lib.Day10
open NUnit.Framework
open Swensen.Unquote

type Day10Tests() =
    
    [<Test>]
    member this.``get total min light button press count``() =
        test <@ (getTotalMinLightButtonPressCount
                    [ "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
                      "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
                      "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" ] = 7) @>    

    [<Test>]
    member this.``get total min joltage button press count``() =
        test <@ (getTotalMinJoltageButtonPressCount
                    [ "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
                      "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
                      "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" ] = 33) @>