namespace AoC2025.Tests

open AoC2025.Lib.Day05
open NUnit.Framework
open Swensen.Unquote

type Day05Tests() =
    
    [<Test>]
    member this.``count valid ingredients``() =
        test <@ (countValidIngredients
                    [ "3-5"
                      "10-14"
                      "16-20"
                      "12-18"
                      ""
                      "1"
                      "5"
                      "8"
                      "11"
                      "17"
                      "32"] = 3) @>
        
    [<Test>]
    member this.``count valid ingredient ids``() =
        test <@ (countValidIngredientIds
                    [ "3-5"
                      "10-14"
                      "16-20"
                      "12-18"
                      ""
                      "1"
                      "5"
                      "8"
                      "11"
                      "17"
                      "32"] = 14) @>
    
    