module Day12

open AoClib

let testInput =
    """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"""

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput else lines
    let nr = lines.Length
    let nc = lines.Head.Length
    let plots = lines |> array2D

    let rPlots = plots |> Array2D.map (fun p -> -1)
    let mutable currentRegion = -1
    let neighbors nr nc (r, c) =
        [
            if r > 0 then r - 1, c
            if r < nr - 1 then r + 1, c
            if c > 0 then r, c - 1
            if c < nc - 1 then r, c + 1
        ]
    let markR r c plant =
        if rPlots[r, c] < 0 then
            currentRegion <- currentRegion + 1
            let rec mark(r, c) =
                rPlots[r, c] <- currentRegion
                neighbors nr nc (r, c) |> List.filter (fun (r, c) -> plots[r, c] = plant && rPlots[r, c] < 0) |> List.iter mark
            mark(r, c)
    plots |> Array2D.iteri markR

    let areas = Array.create (currentRegion + 1) 0
    rPlots |> Array2D.iter (fun region -> areas[region] <- areas[region] + 1)

    let perimeters = Array.create (currentRegion + 1) 0
    let addToPerimeters r c rp =
        let add () = perimeters[rp] <- perimeters[rp] + 1
        if r = 0 || rPlots[r - 1, c] <> rp then add()
        if r = nr - 1 || rPlots[r + 1, c] <> rp then add()
        if c = 0 || rPlots[r, c - 1] <> rp then add()
        if c = nc - 1 || rPlots[r, c + 1] <> rp then add()
    rPlots |> Array2D.iteri addToPerimeters
    let result1 = [|areas; perimeters|] |> Array.transpose |> Array.sumBy (Array.reduce (*))

    let perimeters2 = Array.create (currentRegion + 1) 0
    let addToPerimeters2 r c rp =
        let add () = perimeters2[rp] <- perimeters2[rp] + 1
        if (r = 0 || rPlots[r - 1, c] <> rp) && (c = 0 || rPlots[r, c - 1] <> rp || r <> 0 && rPlots[r - 1, c - 1] = rp) then add()
        if (r = nr - 1 || rPlots[r + 1, c] <> rp) && (c = 0 || rPlots[r, c - 1] <> rp || r <> nr - 1 && rPlots[r + 1, c - 1] = rp) then add()
        if (c = 0 || rPlots[r, c - 1] <> rp) && (r = 0 || rPlots[r - 1, c] <> rp || c <> 0 && rPlots[r - 1, c - 1] = rp) then add()
        if (c = nc - 1 || rPlots[r, c + 1] <> rp) && (r = 0 || rPlots[r - 1, c] <> rp || c <> nc - 1 && rPlots[r - 1, c + 1] = rp)  then add()
    rPlots |> Array2D.iteri addToPerimeters2
    let result2 = [|areas; perimeters2|] |> Array.transpose |> Array.sumBy (Array.reduce (*))


    result1, result2