namespace AoC2025.Tests

open AoC2025.Lib.Day11
open NUnit.Framework
open Swensen.Unquote

type Day11Tests() =

    [<Test>]
    member this.``count paths from you to out``() =
        test <@ (countPathsFromYouToOut
                    [ "aaa: you hhh"
                      "you: bbb ccc"
                      "bbb: ddd eee"
                      "ccc: ddd eee fff"
                      "ddd: ggg"
                      "eee: out"
                      "fff: out"
                      "ggg: out"
                      "hhh: ccc fff iii"
                      "iii: out" ] = 5) @>

    [<Test>]
    member this.``count valid paths from svr to out``() =
        test <@ (countValidPathsFromSvrToOut
                    [ "svr: aaa bbb"
                      "aaa: fft"
                      "fft: ccc"
                      "bbb: tty"
                      "tty: ccc"
                      "ccc: ddd eee"
                      "ddd: hub"
                      "hub: fff"
                      "eee: dac"
                      "dac: fff"
                      "fff: ggg hhh"
                      "ggg: out"
                      "hhh: out" ] = 2) @>
