module LongestSequenceOf1s

let solve (b:string) =
    b.Split('0')
    |> Array.map (fun s -> s.Length)
    |> Array.pairwise
    |> Array.fold (fun acc (a, b) -> a + b + 1 |> max acc) 0