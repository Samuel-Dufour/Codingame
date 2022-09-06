module GhostLegs

type Line = { Name: char; EndName: char }

module Line =
    let mapper name endName = { Name = name; EndName = endName }

type Link = Link of start: Line * endd: Line
type Row = Row of idx: int * links: Link array

let solve lines =
    let lines = lines |> Seq.skip 1
    let starts =
        let names =
            lines
            |> Seq.head
            |> Seq.filter ((<>) ' ')

        let ends =
            lines
            |> Seq.last
            |> Seq.filter ((<>) ' ')

        Seq.map2 Line.mapper names ends |> Array.ofSeq


    let rows =
        let splitToLinks (s: string) =
            s.Split([| "|" |], System.StringSplitOptions.RemoveEmptyEntries)

        let toLink cidx s =
            match s with
            | "--" ->
                Some(
                    [| Link(starts.[cidx], starts.[cidx + 1])
                       Link(starts.[cidx + 1], starts.[cidx]) |]
                )
            | _ -> None

        let toLinks =
            Array.mapi toLink
            >> Array.choose id
            >> Array.collect id

        let toRow idx s = idx, toLink s

        let mapper ridx s = Row(ridx, splitToLinks s |> toLinks)

        let lastIdx = lines |> Seq.length |> (+) -1

        lines
        |> Seq.skip 1
        |> Seq.take (lastIdx - 1)
        |> Seq.mapi mapper
        |> Array.ofSeq

    let computeLine line =
        let ({ Name = name; EndName = endName }) = line

        let folder acc (Row (_, links)) =
            match links
                  |> Array.tryFind (fun (Link ({ Name = s }, _)) -> s = acc.Name)
                with
            | None -> acc
            | Some (Link (_, e)) -> e

        rows |> Array.fold folder line

    starts
    |> Array.map (fun (s) -> sprintf "%c%c" s.Name (computeLine s).EndName)
