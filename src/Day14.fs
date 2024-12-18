module Day14

open AoClib

#nowarn 25

let testInput =
    """p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"""

let getResults (lines: string list, example: string) =
    let lines, nr, nc = if example = "1" then splitLines testInput, 7, 11 else lines, 103, 101
    let getPV (line: string) =
        line.Split(" ")
        |> Array.toList
        |> List.map (fun (pv: string) -> let a = pv.Split("=").[1].Split(",") in (int a[1], int a[0]))
    let startRobots = lines |> List.map (getPV >> fun pv -> pv.Head, pv.Tail.Head)
    let show robots =
        let a = Array2D.create nr nc false
        robots |> List.iter (fun ((r, c), _) -> a[r, c] <- true)
        for c in 0 .. nc - 1 do
            printfn "%s" (a[*, c] |> Array.map (fun on -> if on then '*' else ' ') |> System.String)
    let rec update1 n (((r, c), (vr, vc)) as robot) =
        if n = 0 then robot else update1 (n - 1) (((r + vr + nr) % nr, (c + vc + nc) % nc), (vr, vc))
    let update n = List.map (update1 n)
    let score robots =
        let incr m p = Map.change p (function None -> Some 1 | Some n -> Some (n + 1)) m
        (Map.empty, List.map fst robots)
            ||> List.fold incr
            |> Map.toList
            |> List.groupBy (fun ((r, c), _) -> compare r (nr/2), compare c (nc/2))
            |> List.filter (fun ((c1, c2), _) -> c1 <> 0 && c2 <> 0)
            |> List.map (snd >> List.sumBy snd)
            |> List.reduce (*)
    
    // startRobots |> update (43 + 63 * 103) |> show
    // (update 43 startRobots, [0 .. 1000]) ||> List.fold (fun robots i -> printfn $"{i}:"; show robots; update 103 robots) |> ignore

    score (update 100 startRobots), 6532
 