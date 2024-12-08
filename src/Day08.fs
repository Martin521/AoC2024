module Day08

open AoClib

let testInput =
    """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""

type Parts = Part1 | Part2

let getResults (lines: string list) =
    let lines = if useExample then splitLines testInput else lines
    let nr = lines.Length
    let nc = lines.Head.Length
    let antennas =
        lines
        |> List.mapi (fun r -> Seq.mapi (fun c a -> a, (r, c)) >> Seq.toList)
        |> List.concat
        |> List.groupBy fst
        |> List.filter (fst >> (<>) '.')
        |> List.map (snd >> List.map snd)
    let rec getGroupAntipodes dayPart antennaGroup =
        match antennaGroup with
        | [] -> Set.empty
        | (r1, c1) :: groupRemainder ->
            let getAntipodes (r2, c2) =
                let dr, dc = r1 - r2, c1 - c2
                let inBounds (r, c) = r >= 0 && r < nr && c >= 0 && c < nc
                match dayPart with
                | Part1 ->
                    Set [
                        if inBounds (r1 + dr, c1 + dc) then r1 + dr, c1 + dc
                        if inBounds (r2 - dr, c2 - dc) then r2 - dr, c2 - dc
                    ]
                | Part2 ->
                    Set [
                        let rec getInBounds r c dr dc =
                            if inBounds (r, c) then (r, c) :: getInBounds (r - dr) (c - dc) dr dc else []
                        yield! getInBounds r1 c1 dr dc
                        yield! getInBounds r2 c2 -dr -dc
                    ]
            let antennaAntipodes = groupRemainder |> List.map getAntipodes |> Set.unionMany
            antennaAntipodes + getGroupAntipodes dayPart groupRemainder
    let getCount dayPart = antennas |> List.map (getGroupAntipodes dayPart) |> Set.unionMany |> Set.count

    getCount Part1, getCount Part2
