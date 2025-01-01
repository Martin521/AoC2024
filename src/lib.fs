module AoClib

open System

let splitLines (testInput: string) =
    testInput.ReplaceLineEndings(Environment.NewLine).Split(Environment.NewLine) |> Array.toList

let splitLine (separator: string) (line: string) = line.Split separator |> Array.toList

let splitString (splitter: string) (s: string) = s.Split splitter |> Array.toList

let startsWith (s: string) (x: string) = x.StartsWith s

let list2pair xs = List.head xs, xs.Tail.Head

let findCharPos x = Seq.indexed  >> Seq.find (snd >> Seq.contains x) >> fun(r, row) -> r, Seq.findIndex ((=) x) row

let mkCharMap (lines: string list) = lines |> Seq.mapi (fun r -> Seq.mapi (fun c x -> (r, c), x)) |> Seq.concat |> Map

let mkCharPosSet chr (lines: string list) =
    lines
    |> Seq.mapi (fun r -> Seq.mapi (fun c x -> if x = chr then Some(r, c) else None))
    |> Seq.concat
    |> Seq.choose id
    |> Set

let allPositions nr nc = [
    for r in 0 .. nr - 1 do
        for c in 0 .. nc - 1 do
            yield r, c
]

let allInnerPositions nr nc = [
    for r in 1 .. nr - 2 do
        for c in 1 .. nc - 2 do
            yield r, c
]

let largeInt = System.Int32.MaxValue / 2

let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

let rec gcd x y = if y = 0L then x else gcd y (x % y)
let lcm x y = x * y / gcd x y

let memoize fn =
    let cache =
        new Collections.Generic.Dictionary<_, _>(StringComparer.Ordinal)
    fun x ->
        match cache.TryGetValue x with
        | true, v ->
            v
        | false, _ ->
            let v = fn (x)
            cache.Add(x, v)
            v
