namespace AoC2025.Tests

open AoC2025.Lib.Day02
open NUnit.Framework
open Swensen.Unquote

type Day02Tests() =
    
    [<TestCase("95-115", "95-99")>]
    [<TestCase("5426-11501", "5426-9999")>]
    [<TestCase("446443-446449", "446443-446449")>]
    member this.``make range even length``(input, output)  =
        test <@ parseRange input
                |> adjustRangeLength 2
                |> Option.get = parseRange output  @>

    [<Test>]
    member this.``calculate sum of invalid ids repeated twice``() =
        test <@ calculateSumOfInvalidIdsRepeatedTwice
                    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124" = 1227775554 @>

    [<Test>]
    member this.``calculate sum of invalid ids repeated more than twice``() =
        test <@ calculateSumOfInvalidIdsRepeatedMoreThanTwice
                    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124" = 4174379265L @>
