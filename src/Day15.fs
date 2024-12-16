module Day15

open AoClib

let testInput =
    """##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"""

let smallTestInput = """########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"""

let smallTestInput2 = """#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^"""

type Dir = Up | Down | Right | Left with
    static member fromChar = function '^' -> Up | 'v' -> Down | '<' -> Left | '>' -> Right | _ -> failwith "invalid d"

let getResults (lines: string list) =
    let lines = if useExample then splitLines testInput else lines
    let tilesList = lines |> List.takeWhile (not << System.String.IsNullOrEmpty) |> List.map Seq.toList

    let tiles = tilesList |> List.map List.toArray |> List.toArray
    let moves =
        lines
        |> List.skipWhile (not << System.String.IsNullOrEmpty)
        |> List.tail
        |> List.collect (Seq.map Dir.fromChar >> Seq.toList)
    let robotPosition tiles =
        let colIndices = tiles |> Array.map (Array.tryFindIndex ((=) '@'))
        let rowIndex = colIndices |> Array.findIndex Option.isSome
        rowIndex, colIndices[rowIndex].Value
    let update robotPosition move =
        let tile (r, c) = tiles[r][c]
        let setTile (r, c) t = tiles[r][c] <- t
        let shift nextPos pos =
            setTile nextPos (tile pos)
            setTile pos '.'
        let next (r, c) =
            match move with
            | Up -> r - 1, c
            | Down -> r + 1, c
            | Left -> r, c - 1
            | Right -> r, c + 1
        let rec tryShift pos =
            let nextPos = next pos
            match tile nextPos with
                | '@' -> failwith "unexpected: another robot!"
                | 'O' -> if tryShift nextPos then shift nextPos pos; true else false
                | '#' -> false
                | '.' -> shift nextPos pos; true
                | c -> failwith $"unexpected char '{c}'"
        if tryShift robotPosition then next robotPosition else robotPosition
    (robotPosition tiles, moves) ||> List.fold update |> ignore
    let result1 =
        tiles
        |> Array.mapi (fun r -> Array.mapi (fun c t -> if t = 'O' then 100 * r + c else 0))
        |> Array.sumBy Array.sum

    let widen t =
        match t with
        | '@' -> ['@'; '.']
        | 'O' -> ['['; ']']
        | c -> List.replicate 2 c
    let tilesList = tilesList |> List.map (List.collect widen)
    let nr, nc = tilesList.Length, tilesList.Head.Length
    let tiles = tilesList |> List.mapi (fun r -> (List.mapi (fun c t -> (r, c), t))) |> List.concat |> Map
    let robotStartPosition = tiles |> Map.pick (fun pos -> function '@' -> Some pos | _ -> None)
    let update (tiles: Map<int*int, char>, robotPosition) move =
        let next (r, c) =
            match move with
            | Up -> r - 1, c
            | Down -> r + 1, c
            | Left -> r, c - 1
            | Right -> r, c + 1
        let shift tiles pos =
            assert (Map.find (next pos) tiles = '.')
            tiles |> Map.add (next pos) tiles[pos] |> Map.add pos '.'
        let right (r, c) = (r, c + 1)
        let left (r, c) = (r, c - 1)
        let rec nextBoxes tiles boxPositions =
            match boxPositions with
            | [] -> Some Set.empty
            | pos::t ->
                let nextPos = next pos
                let add positions = nextBoxes tiles t |> Option.map (Set.union (Set positions))
                match move, Map.find nextPos tiles with
                    | _, '#' -> None
                    | _, '.' -> nextBoxes tiles t
                    | (Right | Left), ('[' | ']') -> add [nextPos]
                    | (Up | Down), '[' -> add [nextPos; right nextPos]
                    | (Up | Down), ']' -> add [nextPos; left nextPos]
                    | _, c -> failwith $"unexpected char '{c}'"
        let rec tryShift tiles positions =
            match positions with
            | [] -> Some tiles
            | _ ->
                match nextBoxes tiles positions with
                | None -> None
                | Some boxSet ->
                    let boxes = Set.toList boxSet
                    match tryShift tiles boxes with
                    | None -> None
                    | Some tiles -> (tiles, boxes) ||> List.fold shift |> Some
        match tryShift tiles [robotPosition] with
        | Some tiles -> shift tiles robotPosition, next robotPosition
        | None -> tiles, robotPosition
    let tiles, _ = ((tiles, robotStartPosition), moves) ||> List.fold update
    let result2 = tiles |> Map.toList |> List.sumBy (fun ((r, c), t) -> if t = '[' then 100 * r + c else 0)


    result1, result2
