module OffsetArrays

open System.Text.RegularExpressions

module Parser =
    let (|Integer|_|) (str: string) =
        let mutable intvalue = 0

        if System.Int32.TryParse(str, &intvalue) then
            Some(intvalue)
        else
            None

    let (|Integers|_|) (str: string) =
        let values = str.Split(" ") |> Array.choose (|Integer|_|)

        if Array.isEmpty values then
            None
        else
            Some values

    let (|ParseRegex|_|) regex str =
        let m = Regex(regex).Match(str)

        if m.Success then
            Some(List.tail [ for x in m.Groups -> x.Value ])
        else
            None

    let (|ParseRegexGroupName|_|) regex (name: string) str =
        let regex = sprintf regex name

        let m = Regex(regex).Matches(str)

        if Seq.isEmpty m then
            None
        else
            m
            |> Seq.map (fun m -> m.Groups[name].Value)
            |> List.ofSeq
            |> Some

    let (|ArrayDefinition|_|) s =
        match s with
        | ParseRegex @"^(?<name>\w+)\[(?<fstIdx>-?\d+)..(?<lstIdx>-?\d+)\] = (?<values>[-\d\s]+)"
                     [ name; Integer fstIdx; Integer lstIdx; Integers values ] ->
            Some(ArrayDefinition(name, fstIdx, lstIdx, values))
        | _ -> None

    let (|ReadExp|_|) s =
        let tryIndex s =
            match s with
            | ParseRegex @"(?<index>-?\d+)" [ Integer idx ] -> Some idx
            | _ -> None

        let idx = tryIndex s

        match idx, s with
        | Some idx, ParseRegexGroupName @"\[?(?<%s>[a-zA-Z]+)" "name" t -> ReadExp(idx, List.rev t) |> Some
        | _ -> None

type IndexArray = IndexArray of name: string * values: Map<int, int>

module IndexArray =
    let create name (fstIdx, lstIdx) values =
        let map =
            Seq.map2 (fun idx value -> idx, value) [ fstIdx..lstIdx ] values
            |> Seq.fold (fun (acc: Map<int, int>) (idx, value) -> acc.Add(idx, value)) Map.empty

        IndexArray(name, map)

    let find idx (IndexArray (_, map)) = Map.find idx map

type ReadExpression = ReadExpression of fstIdx: int * names: string list

type Line =
    | IndexArrayDefinition of IndexArray
    | ReadExpressionDefinition of ReadExpression
    | Other

[<RequireQualifiedAccess>]
module Line =
    open Parser

    let fromString line =
        match line with
        | ArrayDefinition (name, fstIdx, lstIdx, values) ->
            IndexArray.create name (fstIdx, lstIdx) values
            |> IndexArrayDefinition
        | ReadExp (fstIdx, names) ->
            ReadExpression(fstIdx, names)
            |> ReadExpressionDefinition
        | _ -> Other

module Solver =
    let toArraysMap lines =
        lines
        |> Seq.choose (fun d ->
            match d with
            | IndexArrayDefinition (IndexArray (idx, _) as ia) -> Some(idx, ia)
            | _ -> None)
        |> Seq.fold (fun map (k, v) -> Map.add k v map) Map.empty

    let toReadExp lines =
        lines
        |> Seq.choose (fun d ->
            match d with
            | ReadExpressionDefinition re -> Some re
            | _ -> None)
        |> Seq.head


    let solve lines =
        let lines = lines |> Seq.map Line.fromString
        let arraysMap = toArraysMap lines
        let (ReadExpression (fstIdx, names)) = toReadExp lines

        let rec loop arrayNames idx =
            match arrayNames with
            | [] -> idx
            | head :: tail ->
                arraysMap
                |> Map.find head
                |> IndexArray.find idx
                |> loop tail

        loop names fstIdx
