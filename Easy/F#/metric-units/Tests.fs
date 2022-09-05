module Tests

open Xunit
open Swensen.Unquote
open MetricUnits
open System.IO

[<Theory>]
[<InlineData("1m + 1cm", "101cm")>]
[<InlineData("459m + 132m", "591m")>]
[<InlineData("1km + 14dm", "10014dm")>]
[<InlineData("0.02km + 450mm", "20450mm")>]
[<InlineData("2.55m + 35cm", "290cm")>]
[<InlineData("0.22532m + 90mm", "315.32mm")>]
[<InlineData("0.00098cm + 10mm", "10.0098mm")>]
[<InlineData("0.01dm + 3210um", "4210um")>]
[<InlineData("16.0408mm + 11um", "16051.8um")>]
let ``Solver works for all test cases`` (expression, awaited) =
    test <@ (expression |> Solver.calculate) = awaited @>
