module Day06

open AoClib

let testInput =
    """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput else lines
    let nr = lines.Length
    let nc = lines.Head.Length
    let obstacles = lines |> mkCharPosSet '#'
    let startRow = lines |> List.findIndex (Seq.exists (fun c -> c = '^'))
    let startCol = lines[startRow] |> Seq.findIndex (fun c -> c = '^')
    let startPos = ((startRow, startCol), 0)
    // N=0 E=1 S=2 W=3
    let turn d = (d + 1) % 4
    let reverse d = (d + 2) % 4
    let rec next obstacles ((r, c), d) =
        let r', c' =
            match d with
            | 0 -> r - 1, c
            | 1 -> r, c + 1
            | 2 -> r + 1, c
            | 3 -> r, c - 1
            | _ -> failwith "unexpected direction"
        if r' < 0 || r' >= nr || c' < 0 || c' >= nc then None
        elif Set.contains (r', c') obstacles then next obstacles ((r, c), turn d)
        else Some((r', c'), d)

    let rec patrol visited pos =
        match next obstacles pos with
        | None -> pos :: visited
        | Some p -> patrol (pos :: visited) p
    let visited = patrol [] startPos |> List.rev
    let result1 = visited |> List.map fst |> Set |> Set.count

    let obsList = obstacles |> Set.toList
    let getMaps ((r, c) as p) =
        let rec getMaps n0 t0 n1 t1 n2 t2 n3 t3 obsList =
            match obsList with
            | [] ->
                let forwardMaps =
                    [List.tryLast t0; List.tryLast t1; List.tryHead t2; List.tryHead t3]
                    |> List.mapi (fun d t -> (p, d), Option.map (fun tt -> tt, turn d) t)
                let backwardMaps =
                    [
                    List.filter (fun (_, c) -> c < n0) t0
                    List.filter (fun (r, _) -> r < n1) t1
                    List.filter (fun (_, c) -> c > n2) t2
                    List.filter (fun (r, _) -> r > n3) t3
                    ]
                    |> List.mapi (fun d -> List.map (fun t -> (t, reverse d), Some (p, turn (reverse d))))
                    |> List.concat
                forwardMaps, backwardMaps
            | (ro, co) as po :: t ->
                match ro - r, co - c with
                | -1, dc when dc < 0 ->                     // coming in southbound (2), turning west
                    getMaps n0 t0 n1 t1 n2 (po::t2) n3 t3 t
                | 0, dc when dc < 0 ->                      // western neighbor
                    getMaps n0 t0 n1 t1 co t2 n3 t3 t
                | 0, dc when dc > 0 && co < n0 ->           // eastern neighbor
                    getMaps co t0 n1 t1 n2 t2 n3 t3 t
                | 1, dc when dc > 0 ->                      // coming in northbound (0), turning east
                    getMaps n0 (po::t0) n1 t1 n2 t2 n3 t3 t
                | dr, -1 when dr > 0 ->                     // coming in eastbound (1), turning south
                    getMaps n0 t0 n1 (po::t1) n2 t2 n3 t3 t
                | dr, 0 when dr > 0 && ro < n1 ->           // southern neighbor
                    getMaps n0 t0 ro t1 n2 t2 n3 t3 t
                | dr, 0 when dr < 0 ->                      // northern neighbor
                    getMaps n0 t0 n1 t1 n2 t2 ro t3 t
                | dr, 1 when dr < 0 ->                      // coming in westbound (3), turning north
                    getMaps n0 t0 n1 t1 n2 t2 n3 (po::t3) t
                | _  -> getMaps n0 t0 n1 t1 n2 t2 n3 t3 t
        getMaps nc [] nr [] 0 [] 0 [] obsList
    let nextMaps = obsList|> List.collect (getMaps >> fst) |> Map
    let nextMapsWith newObsPos =
        let forwardMaps, backwardMaps = getMaps newObsPos
        (nextMaps, forwardMaps @ backwardMaps) ||> List.fold (fun ms (p, tp) -> Map.add p tp ms)

    let rec isLoop nextMaps visited pos =
        if Set.contains pos visited then
            true
        else
            match Map.find pos nextMaps with
            | None -> false
            | Some p -> isLoop nextMaps (visited.Add pos) p
    let rec countLoops createsLoop remaining =
        match remaining with
        | [] -> failwith "unexpected: empty in countLoops"
        | [_] -> createsLoop |> Map.values |> Seq.filter id |> Seq.length
        | _ :: (((obsPos, d) :: _) as t) ->
            let createsLoop =
                if createsLoop.ContainsKey obsPos then
                    createsLoop
                else
                    createsLoop.Add(obsPos, isLoop (nextMapsWith obsPos) Set.empty (obsPos, d))
            countLoops createsLoop t
    let result2 = countLoops Map.empty visited

    result1, result2
