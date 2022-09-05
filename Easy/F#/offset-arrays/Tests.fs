module Tests

open Xunit
open Swensen.Unquote
open OffsetArrays
open System.IO

[<Theory>]
[<InlineData("input-1.txt", 2)>]
[<InlineData("input-2.txt", 22)>]
[<InlineData("input-3.txt", 7)>]
[<InlineData("input-4.txt", -69)>]
[<InlineData("input-5.txt", 1547)>]
[<InlineData("input-6.txt", 506)>]
[<InlineData("input-7.txt", 24184051)>]
[<InlineData("input-8.txt", 28)>]
[<InlineData("input-9.txt", -2)>]
[<InlineData("input-10.txt", 400)>]

let ``Solver works for all test cases`` (sampleFile, awaited) =
    let lines = File.ReadAllLines $@"samples\{sampleFile}"
    test <@ (lines |> Solver.solve) = awaited @>
