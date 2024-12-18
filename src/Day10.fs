module Day10

open AoClib

let testInput =
    """89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"""

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput else lines
    let nr = lines.Length
    let nc = lines.Head.Length
    let heights = lines |> List.map (Seq.map (fun c -> int (c - '0')) >> Seq.toArray) |> List.toArray
    let inline height (r, c) = heights[r][c]
    let trailheads =
        lines
        |> List.mapi (fun r -> Seq.mapi (fun c h -> (r, c), h = '0') >> Seq.filter snd >> Seq.toList >> List.map fst)
        |> List.concat
    let emptyStates = Array.init nr (fun _ -> Array.replicate nc None)
    let getS (states: _ array array) (r, c) = states[r][c]
    let setS states s (r, c) = (states |> Array.map (Array.copy)).[r].[c] <- Some s

    let next h (r, c) =
        [
            if r > 0 then r - 1, c
            if r < nr - 1 then r + 1, c
            if c > 0 then r, c - 1
            if c < nr - 1 then r, c + 1
        ]
        |> List.filter (fun (r, c) -> height (r, c) = h + 1)

    let rec getState merge states pos =
        match getS states pos with
        | Some state -> state
        | None ->
            let state =
                match height pos with
                | 9 -> Map[pos, 1]
                | h -> next h pos |> List.map (getState merge states) |> merge
            setS states state pos
            state

    let getResult merge =
        trailheads |> List.map (getState merge emptyStates) |> List.sumBy (Map.toList >> List.sumBy snd)

    let merge1 = Seq.collect Map.keys >> Set >> Set.toList >> List.map (fun p -> p, 1) >> Map

    let merge2 =
        List.collect Map.toList >> List.groupBy fst >> List.map (fun (p, hills) -> p, List.sumBy snd hills) >> Map

    getResult merge1, getResult merge2
