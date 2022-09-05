module Tests

open Xunit
open Swensen.Unquote
open SudokuValidator
open System.IO

[<Theory>]
[<InlineData("input-1.txt", true)>]
[<InlineData("input-2.txt", true)>]
[<InlineData("input-3.txt", false)>]
[<InlineData("input-4.txt", false)>]
[<InlineData("input-5.txt", false)>]
[<InlineData("input-6.txt", false)>]
[<InlineData("input-7.txt", false)>]
let ``Solver works for all test cases`` (sampleFile, awaited) =
    let lines = File.ReadAllLines $@"samples\{sampleFile}"
    test <@ lines |> Sudoku.createGrid |> Sudoku.isSudokuValid = awaited @>
