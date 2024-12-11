module Day11

open AoClib

let testInput =
    """125 17"""

let getResults (lines: string list) =
    let lines = if useExample then splitLines testInput else lines
    let numbers = lines.Head.Split(" ") |> Seq.map (fun s -> 1L, int64 s) |> Seq.toList
    let rec getDigitsR = function 0L -> [] | n -> n % 10L :: getDigitsR (n / 10L)
    let rec getNumberR = function [] -> 0L | h::t -> h + 10L * (getNumberR t)
    let split (m, n) =
        match getDigitsR n with
        | [] -> [m, 1L]
        | dsr ->
            match (List.length dsr) % 2 with
            | 0 -> dsr |> List.splitInto 2 |> List.map (fun n -> m, getNumberR n) |> List.rev
            | _ -> [m, 2024L * n]
    let rec blink k ns =
        match k with
        | 0 -> ns |> List.sumBy fst
        | _ ->
            ns
            |> List.collect split
            |> List.groupBy snd
            |> List.map (fun (n, ns) -> ns |> List.sumBy fst, n)
            |> blink (k - 1)

    blink 25 numbers, blink 75 numbers
