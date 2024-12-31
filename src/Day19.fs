module Day19

open AoClib

let testInput =
    """r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"""

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput else lines
    let allPatterns = lines.Head.Split(", ") |> Seq.map Seq.toList |> Seq.toList
    let allDesigns = lines |> List.skip 2 |> List.map Seq.toList

    let rec tryRemove (pattern: char list) (design: char list) =
        match pattern, design with
        | [], _ -> Some design
        | p::pt, d::dt when p = d -> tryRemove pt dt
        | _ -> None
    let rec isPossibleWith patterns design =
        match patterns, design with
        | _, [] -> true
        | [], _ -> false
        | p::ps, _ ->
            match tryRemove p design with
            | Some d when isPossibleWith allPatterns d -> true
            | _ -> isPossibleWith ps design
    let result1 = allDesigns |> List.filter (isPossibleWith allPatterns) |> List.length

    let allPatternsWith n = allPatterns |> List.map (fun p -> p, n)
    let rec numberOfWays endCount patterns design =
        match design with
        | [] -> endCount
        | h::t ->
            let patternEnds, nextPatterns =
                patterns @ (allPatternsWith endCount)
                |> List.choose (fun (p, c) -> if List.head p = h then Some (p.Tail, c) else None)
                |> List.partition (fst >> List.isEmpty)
            numberOfWays (List.sumBy snd patternEnds) nextPatterns t
    let result2 = allDesigns |> List.sumBy (numberOfWays 1L [])

    int64 result1, result2
