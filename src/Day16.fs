module Day16

open AoClib

#nowarn 104

let testInput1 =
    """###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"""

let testInput2 = """#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"""

type D = N = 0 | S = 1 | E = 2 | W = 3
let allDirections = [D.N; D.S; D.E; D.W]

let rec mergeBy f xs ys =
    match xs, ys with
    | [], _ -> ys
    | _, [] -> xs
    | x::xt, y::yt -> if f x < f y then x :: mergeBy f xt ys else y :: mergeBy f xs yt

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput1 elif example = "2" then splitLines testInput2 else lines
    let bigScore = System.Int32.MaxValue / 2
    let createDirArray score = Array.create allDirections.Length (score, Set.empty)
    let perDir c = match c with '#' -> createDirArray -1 | _ -> createDirArray bigScore
    let scoredSeats = lines |> List.map (Seq.map perDir >> Seq.toArray) |> List.toArray
    let getScoredSeats ((r, c), d) = scoredSeats[r].[c].[int d]
    let setScoredSeats ((r, c), d) score = scoredSeats[r].[c].[int d] <- score
    let startPosition = lines.Length - 2, 1
    let endPosition = 1, lines.Head.Length - 2
    let startDirection = D.E
    setScoredSeats (startPosition, startDirection) (0, Set[startPosition])
    let next (r, c) = function D.N -> r - 1, c | D.S -> r + 1, c | D.E -> r, c + 1 | D.W -> r, c - 1
    let dirPenalties d =
        let turns = match d with D.N | D.S -> [D.E; D.W] | D.E | D.W -> [D.N; D.S]
        (d, 1) :: List.map (fun d -> d, 1001) turns
    let mutable minEndScore = bigScore
    let rec explore tiles =
        match tiles with
        | [] -> ()
        | (pos, dir) :: remainder ->
            let score, seats = getScoredSeats (pos, dir)
            if pos = endPosition && score < minEndScore then minEndScore <- score
            let newTiles =
                dirPenalties dir
                |> List.choose (fun (nextDir, p) ->
                    let nextPos = next pos nextDir
                    let nextScore = score + p
                    if minEndScore < nextScore then None else
                    let prevNextScore, prevNextSeats = getScoredSeats (nextPos, nextDir)
                    if nextScore < prevNextScore then
                        setScoredSeats (nextPos, nextDir) (nextScore, Set.add nextPos seats)
                        Some (nextPos, nextDir)
                    elif nextScore = prevNextScore then
                        if not (seats - prevNextSeats).IsEmpty then
                            setScoredSeats (nextPos, nextDir) (nextScore, prevNextSeats + seats)
                            Some (nextPos, nextDir)
                        else None
                    else None)
            let allTiles = newTiles |> List.sortBy getScoredSeats |> mergeBy getScoredSeats remainder
            explore allTiles
    explore [startPosition, startDirection]

    let endPositions = allDirections |> List.map (fun d -> endPosition, d)
    let result1 = endPositions |> List.map (getScoredSeats >> fst) |> List.min
    let result2 = endPositions |> List.map getScoredSeats |> List.minBy fst |> snd |> Set.count

    result1, result2
