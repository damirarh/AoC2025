namespace AoC2025.Tests

open AoC2025.Lib.Day06
open NUnit.Framework
open Swensen.Unquote

type Day06Tests() =
    
    [<Test>]
    member this.``calculate grand total``() =
        test <@ (calculateGrandTotal
                    [ "123 328  51 64" 
                      " 45 64  387 23" 
                      "  6 98  215 314"
                      "*   +   *   +"] = 4277556L) @>    
    
    [<Test>]
    member this.``calculate grand total right to left``() =
        test <@ (calculateGrandTotalRightToLeft
                    [ "123 328  51 64" 
                      " 45 64  387 23" 
                      "  6 98  215 314"
                      "*   +   *   +"] = 3263827) @>