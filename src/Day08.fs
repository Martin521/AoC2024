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

let getResults (lines: string list) =
    let lines = if useExample then splitLines testInput else lines
    let nr = lines.Length
    let nc = lines.Head.Length
    let inBounds (r, c) = r >= 0 && r < nr && c >= 0 && c < nc
    let antennas =
        lines
        |> List.mapi (fun r -> Seq.mapi (fun c a -> a, (r, c)))
        |> Seq.concat
        |> Seq.groupBy fst
        |> Map
        |> Map.remove '.'
        |> Map.toList
        |> List.map (snd >> Seq.map snd >> Set)
    let rec getAntipodeSet single aSet =
        if Set.isEmpty aSet then Set.empty else
        let (rn, cn) = aSet.MinimumElement
        let rSet = aSet.Remove(rn, cn)
        let getAntipodes (ra, ca) =
            let dr, dc = ra - rn, ca - cn
            if single then
                Set [
                    if inBounds (ra + dr, ca + dc) then ra + dr, ca + dc
                    if inBounds (rn - dr, cn - dc) then rn - dr, cn - dc
                ]
            else
                Set [
                    let rec getInBounds r c dr dc =
                        if inBounds (r, c) then (r, c) :: getInBounds (r + dr) (c + dc) dr dc else []
                    yield! getInBounds ra ca dr dc
                    yield! getInBounds rn cn -dr -dc
                ]
        let nextAntipodes = rSet |> Set.map getAntipodes |> Set.unionMany
        nextAntipodes + getAntipodeSet single rSet
    let getCount single = antennas |> List.map (getAntipodeSet single) |> Set.unionMany |> Set.count

    getCount true, getCount false
