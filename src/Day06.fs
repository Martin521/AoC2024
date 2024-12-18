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

type Direction =
    | N
    | S
    | E
    | W

let directionCount = 4

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput else lines
    let nr = lines.Length
    let nc = lines.Head.Length
    let obstacles = lines |> List.map (Seq.map (fun c -> c = '#') >> Seq.toArray) |> List.toArray
    let visited = obstacles |> Array.map (Array.map (fun _ -> false))
    let startRow = lines |> List.findIndex (Seq.exists (fun c -> c = '^'))
    let startCol = lines[startRow] |> Seq.findIndex (fun c -> c = '^')
    let index = function N -> 0 | S -> 1 | E -> 2 | W -> 3
    let turn = function N -> E | E -> S | S -> W | W -> N
    let rec next (r, c, d) =
        let r', c' =
            match d with
            | W -> r, c - 1
            | E -> r, c + 1
            | S -> r + 1, c
            | N -> r - 1, c
        if r' < 0 || r' >= nr || c' < 0 || c' >= nc then None
        elif obstacles[r'][c'] then next (r, c, turn d) else Some(r', c', d)
    let rec patrol (r, c, d) =
        visited[r].[c] <- true
        match next (r, c, d) with
        | None -> ()
        | Some(r, c, d) -> patrol (r, c, d)
    patrol (startRow, startCol, N)
    let result1 = visited |> Array.sumBy (Array.sumBy (fun v -> if v then 1 else 0))

    let visited2 = Array3D.create nr nc directionCount false
    let rec guardIsCircling (r, c, (d: Direction)) =
        if visited2[r, c, index d] then
            true
        else
            visited2[r, c, index d] <- true
            match next (r, c, d) with
            | None -> false
            | Some(r, c, d) -> guardIsCircling (r, c, d)
    let mutable count = 0
    for obsr in 0 .. nr - 1 do
        for obsc in 0 .. nc - 1 do
            if visited[obsr][obsc] && not (obstacles[obsr][obsc]) then
                obstacles[obsr][obsc] <- true
                System.Array.Clear visited2
                if guardIsCircling (startRow, startCol, N) then count <- count + 1
                obstacles[obsr][obsc] <- false
    let result2 = count

    result1, result2
