module SudokuValidator

open System

module Sudoku =
    let fromArray (grid: int [] []) =
        Array2D.init 9 9 (fun ridx cidx -> grid.[ridx].[cidx])

    let createGrid (lines: string array) =
        [| for line in lines do
               [| for c in line.Split(' ', StringSplitOptions.RemoveEmptyEntries) do
                      c |> string |> int |] |]
        |> fromArray

    let isSudokuValid (array: int [,]) =
        let isArrayOk (array: int []) =
            let allDigits = [ 1..9 ]
            allDigits |> Seq.except array |> Seq.isEmpty

        let checkLinesAndRows (array: int [,]) =

            let areLinesOk =
                [ for idx in 0 .. Array2D.length1 array - 1 do
                      isArrayOk array.[idx, *] ]
                |> List.forall id

            let areRowsOk =
                [ for idx in 0 .. Array2D.length2 array - 1 do
                      isArrayOk array.[*, idx] ]
                |> List.forall id

            areLinesOk && areRowsOk

        let areSubSquaresOk =
            [ for ridx in 0..3..8 do
                  for cidx in 0..3..8 do
                      let values = ResizeArray<int>()

                      array.[ridx .. ridx + 2, cidx .. cidx + 2]
                      |> Array2D.iter (fun i -> values.Add i)

                      values.ToArray() |> isArrayOk ]
            |> List.forall id

        checkLinesAndRows array && areSubSquaresOk

