module Day18

open AoClib

let testInput =
    """5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"""

let largeInt = System.Int32.MaxValue / 2

let getResults (lines: string list, example) =
    let lines, nr, nc, take = if example = "1" then splitLines testInput, 7, 7, 12 else lines, 71, 71, 1024
    let bytes = lines |> List.map (fun line -> let a = line.Split(",") in int a[1], int a[0])
    let startPos = 0, 0
    let endPos = nr - 1, nc - 1

    let getShortestPathScore numBytesFallen =
        let byteSet = Set(List.take numBytesFallen bytes)
        let getNeighbors (r, c) =
            [
                if r > 0 then r - 1, c
                if r < nr - 1 then r + 1, c
                if c > 0 then r, c - 1
                if c < nc - 1 then r, c + 1
            ]
            |> List.filter (not << byteSet.Contains)
            |> Set
        let rec update score (scores, queue) =
            let update1 (scores, queue) pos =
                match Map.tryFind pos scores with
                | None -> Map.add pos score scores, queue + getNeighbors pos
                | Some prevScore when score < prevScore -> Map.add pos score scores, queue + getNeighbors pos
                | Some _ -> scores, queue
            if Set.isEmpty queue then
                Map.tryFind endPos scores
            else
                update (score + 1) (((scores, Set.empty), queue) ||> Set.fold update1)
        update 0 (Map.empty, Set[startPos])
    let result1 = getShortestPathScore take |> Option.get

    let result2 =
        let rec find f imin imax =
            if imin = imax then
                imin
            else
                let imid = (imax + imin + 1) / 2
                if f imid then find f imin (imid - 1) else find f imid imax
        let i = find (getShortestPathScore >> Option.isNone) take bytes.Length
        let r, c = bytes[i]
        $"{c},{r}"

    string result1, result2
