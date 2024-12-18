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
        let byteSet = Set (List.take numBytesFallen bytes)
        let getNeighbors (r, c) =
            [
                if r > 0 then r-1, c
                if r < nr - 1 then r+1, c
                if c > 0 then r, c-1
                if c < nc - 1 then r, c+1
            ]
            |> List.filter (not << byteSet.Contains)
            |> Set
        let scores = Array2D.create nr nc largeInt
        let getScore (r, c) = scores[r, c]
        let setScore (r, c) s = scores[r, c] <- s
        let rec updateScores score queue =
            let updateAndReturnNeighbors pos =
                if score < getScore pos then
                    setScore pos score
                    getNeighbors pos
                else
                    Set.empty
            if not <| Set.isEmpty queue then
                updateScores (score + 1) (queue |> Set.map updateAndReturnNeighbors |> Set.unionMany)
        updateScores 0 Set[startPos]
        getScore endPos
    let result1 = getShortestPathScore take

    let result2 =
        let rec find f imin imax =
            if imin = imax then
                imin
            else
                let imid = (imax + imin + 1) / 2
                if f imid then find f imin (imid - 1) else find f imid imax
        let i = find (fun nbf -> getShortestPathScore nbf = largeInt) take bytes.Length
        let r, c = bytes[i]
        $"{c},{r}"
    
    
    string result1, result2
