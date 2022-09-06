module Tests

open Xunit
open Swensen.Unquote
open GhostLegs
open System.IO

[<Theory>]
[<InlineData("input-1.txt", "expected-1.txt")>]
[<InlineData("input-2.txt", "expected-2.txt")>]
[<InlineData("input-3.txt", "expected-3.txt")>]
[<InlineData("input-4.txt", "expected-4.txt")>]
[<InlineData("input-5.txt", "expected-5.txt")>]
[<InlineData("input-6.txt", "expected-6.txt")>]
let ``Solver works for all test cases`` (sampleFile, expected) =
    let lines = File.ReadAllLines $@"samples\{sampleFile}"
    let expected = File.ReadAllLines $@"samples\{expected}"

    test <@ (solve lines) |> Array.length > 0 @>
