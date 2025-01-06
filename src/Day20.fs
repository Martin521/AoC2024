module Day20

open AoClib

let testInput =
    """###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"""

let getResults (lines: string list, example) =
    let lines, minGain = if example = "1" then splitLines testInput, 50 else lines, 100
    let nr, nc = lines.Length, lines.Head.Length
    let walls = mkCharPosSet '#' lines
    let tracks = Set(allPositions nr nc) - walls
    let startPos, endPos = findCharPos 'S' lines, findCharPos 'E' lines
    let dS, dN, dE, dW = (1, 0), (-1, 0), (0, 1), (0, -1)
    let steps = [dS; dN; dE; dW]
    let add (dr, dc) (r, c) = r + dr, c + dc

    let rec getTrace trace prevPos pos =
        if pos = endPos then
            pos :: trace
        else
            let nextPos =
                steps
                |> List.map (add pos)
                |> List.filter (fun p -> tracks.Contains p && p <> prevPos)
                |> List.exactlyOne
            getTrace (pos :: trace) pos nextPos
    let trace = getTrace [] startPos startPos
    let iTrace = trace |> List.mapi (fun i (r, c) -> i, r, c)
    let rec skip n = function h::t when n > 0 -> skip (n - 1) t | xs-> xs
    let getCheatCount maxCheatLength =
        let rec getCount trace1 trace2 count =
            match trace1, trace2 with
            | [], _ -> count
            | h::t, [] -> getCount t t count
            | (i1, r1, c1)::t1, (i2, r2, c2)::t2 ->
                let cheatLen = abs (r1 - r2) + abs (c1 - c2)
                let gain = abs (i1 - i2) - cheatLen
                let margin = (max (cheatLen - maxCheatLength) (minGain - gain))
                let ct = if margin <= 0 then count + 1 else count
                getCount trace1 (skip (margin / 2 - 1) t2) ct
        getCount iTrace iTrace 0

    getCheatCount 2, getCheatCount 20
