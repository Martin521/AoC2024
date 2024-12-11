module Day11

open AoClib

let testInput =
    // """0 1 10 99 999"""
    """125 17"""

let getResults (lines: string list) =
    let lines = if useExample then splitLines testInput else lines
    let numbers = lines.Head.Split(" ") |> Seq.map int64 |> Seq.toList
    let rec getDigitsR = function 0L -> [] | n -> n % 10L :: getDigitsR (n / 10L)
    let rec getNumberR = function [] -> 0L | h::t -> h + 10L * (getNumberR t)
    let split n =
        if n = 0L then [1L]
        else
            let dsr = getDigitsR n
            if List.length dsr % 2 = 0 then
                dsr |> List.splitInto 2 |> List.map getNumberR |> List.rev
            else [2024L * n]
    // let rec blink1 ns = function 0 -> ns | k -> blink1 (ns |> List.collect split) (k - 1)
    let rec blinkC caches ns k =
        let caches =
            match caches with
            | [] -> [Map.empty]
            | _ -> caches
        match k with
        | 0 -> [Map.empty], int64 (List.length ns)
        | _ ->
            ((caches, 0L), ns) ||> List.fold (fun (caches, count) n ->
                match Map.tryFind n caches.Head with
                | None -> 
                    let nextCaches, c = blinkC caches.Tail (split n) (k - 1)
                    Map.add n c caches.Head :: nextCaches, count + c
                | Some c -> caches, count + c
            )
    let blink numbers k = blinkC [] numbers k |> snd

    blink numbers 25, blink numbers 75
