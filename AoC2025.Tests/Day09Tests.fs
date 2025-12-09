namespace AoC2025.Tests

open AoC2025.Lib.Day09
open NUnit.Framework
open Swensen.Unquote

type Day09Tests() =

    [<Test>]
    member this.``calculate largest area``() =
        test <@ (calculateLargestArea
                    [ "7,1"
                      "11,1"
                      "11,7"
                      "9,7"
                      "9,5"
                      "2,5"
                      "2,3"
                      "7,3" ] = 50) @>
        
    [<Test>]
    member this.``calculate largest area with red and green tiles only``() =
        test <@ (calculateLargestAreaWithRedAndGreenTilesOnly
                    [ "7,1"
                      "11,1"
                      "11,7"
                      "9,7"
                      "9,5"
                      "2,5"
                      "2,3"
                      "7,3" ] = 24) @>