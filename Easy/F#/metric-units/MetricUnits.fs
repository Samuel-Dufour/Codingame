module MetricUnits

open System.Text.RegularExpressions
open System
open System.Globalization

[<Measure>]
type um

[<Measure>]
type mm

[<Measure>]
type cm

[<Measure>]
type dm

[<Measure>]
type m

[<Measure>]
type km

let umPerMm: float<um / mm> = 1000.0<um/mm>
let mmPerCm: float<mm / cm> = 10.0<mm/cm>
let cmPerDm: float<cm / dm> = 10.0<cm/dm>
let dmPerM: float<dm / m> = 10.0<dm/m>
let mPerKm: float<m / km> = 1000.0<m/km>

type MetricValue =
    | Micro of float<um>
    | Milli of float<mm>
    | Centi of float<cm>
    | Deci of float<dm>
    | Meter of float<m>
    | Kilo of float<km>

    static member (<=>)(a, b) =
        match a, b with
        | Micro _, Micro _ -> true
        | Milli _, Milli _ -> true
        | Centi _, Centi _ -> true
        | Deci _, Deci _ -> true
        | Meter _, Meter _ -> true
        | Kilo _, Kilo _ -> true
        | _ -> false

    static member convertToUnit origin (dest: MetricValue) =
        let rec loop t =
            if t <=> dest then
                t
            else
                match t with
                | Micro _ -> t
                | Milli mm -> Micro(umPerMm * mm) |> loop
                | Centi cm -> Milli(mmPerCm * cm) |> loop
                | Deci dm -> Centi(cmPerDm * dm) |> loop
                | Meter m -> Deci(dmPerM * m) |> loop
                | Kilo km -> Meter(mPerKm * km) |> loop

        loop origin

    static member convertLowerUnit a b =
        if a = b then
            (a, b)
        elif a > b then
            MetricValue.convertToUnit a b, b
        else
            MetricValue.convertToUnit b a, a

    static member (+)(a, b) =
        let rec sum a b =
            match a, b with
            | Micro f, Micro s -> (float f + float s) * 1.<um> |> Micro
            | Milli f, Milli s -> (float f + float s) * 1.<mm> |> Milli
            | Centi f, Centi s -> (float f + float s) * 1.<cm> |> Centi
            | Deci f, Deci s -> (float f + float s) * 1.<dm> |> Deci
            | Meter f, Meter s -> (float f + float s) * 1.<m> |> Meter
            | Kilo f, Kilo s -> (float f + float s) * 1.<km> |> Kilo
            | _ -> MetricValue.convertLowerUnit a b ||> sum

        sum a b


module MetricValue =
    let toString s =
        CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture

        let (value, unit) =
            match s with
            | Micro v -> (float v, "um")
            | Milli v -> (float v, "mm")
            | Centi v -> (float v, "cm")
            | Deci v -> (float v, "dm")
            | Meter v -> (float v, "m")
            | Kilo v -> (float v, "km")

        sprintf "%g%s" value unit

    let value =
        function
        | Micro v -> float v
        | Milli v -> float v
        | Centi v -> float v
        | Deci v -> float v
        | Meter v -> float v
        | Kilo v -> float v

    let toUnit fact s =
        match s with
        | "um" -> fact * 1.<um> |> Micro |> Some
        | "mm" -> fact * 1.<mm> |> Milli |> Some
        | "cm" -> fact * 1.<cm> |> Centi |> Some
        | "dm" -> fact * 1.<dm> |> Deci |> Some
        | "m" -> fact * 1.<m> |> Meter |> Some
        | "km" -> fact * 1.<km> |> Kilo |> Some
        | _ -> None

    let sum values =
        values
        |> Seq.fold (fun a v -> a + v) (Kilo(0.<km>))

module Solver =
    open MetricValue

    let (|ParseRegex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for r in m.Groups.Values -> r.Value ])
        else
            None

    let (|Number|_|) (s: string) =
        CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture

        match System.Double.TryParse s with
        | true, value -> Some value
        | _ -> None


    let parse (line: string) =
        let mapper =
            function
            | ParseRegex @"([\.\d]+)(\w+)" [ Number mult; unit ] -> toUnit mult unit
            | _ -> None

        line.Split(" + ", StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose mapper

    let calculate = parse >> sum >> toString
