module Day21

open AoClib

let testInput =
    """029A
980A
179A
456A
379A"""

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput else lines
    let codes = lines |> List.map (Seq.toList)

    let numKeypad = Map [
        '7', (-3, 0)
        '8', (-3, 1)
        '9', (-3, 2)
        '4', (-2, 0)
        '5', (-2, 1)
        '6', (-2, 2)
        '1', (-1, 0)
        '2', (-1, 1)
        '3', (-1, 2)
        '0', (0, 1)
        'A', (0, 2)
    ]

    let dirKeypad = Map [
        '^', (0, 1)
        'A', (0, 2)
        '<', (1, 0)
        'v', (1, 1)
        '>', (1, 2)
    ]

    let keypads1 = [numKeypad; dirKeypad; dirKeypad]
    let keypads2 = numKeypad :: List.replicate 25 dirKeypad

    let getInnerCount getCount keypads (depth, prevButton, button) =
        match keypads with
        | [] -> 1L
        | keypad::remainingKeypads ->
            let prevRow, prevCol = Map.find prevButton keypad
            let row, col = Map.find button keypad
            let vDiff = row - prevRow
            let hDiff = col - prevCol
            let vMoves = if vDiff < 0 then List.replicate -vDiff '^' else List.replicate vDiff 'v'
            let hMoves = if hDiff < 0 then List.replicate -hDiff '<' else List.replicate hDiff '>'
            match vDiff, hDiff with
            | 0, 0 -> [['A']]
            | 0, _ -> [hMoves @ ['A']]
            | _, 0 -> [vMoves @ ['A']]
            | _ -> [
                if prevRow <> 0 || col <> 0 then hMoves @ vMoves @ ['A']
                if prevCol <> 0 || row <> 0 then vMoves @ hMoves @ ['A']
            ]
            |> List.map (getCount (depth + 1) remainingKeypads 'A')
            |> List.min

    let getCount keypads =
        let cache = memoizationCache()
        let rec getCountR depth (keypads: Map<char,int*int> list) prevButton code =
            let cachedInnerCount = memoizeWith cache (getInnerCount getCountR keypads)
            match code with
            | [] -> 0L
            | h::t -> cachedInnerCount(depth, prevButton, h) + getCountR depth keypads h t
        getCountR 0 keypads 'A'

    let getResult keypads =
        let counts = codes |> List.map (getCount keypads)
        let nums = lines |> List.map (fun line -> line.Substring(0, line.Length - 1) |> System.Int64.Parse)
        List.zip counts nums |> List.sumBy (fun (mvs, num) -> mvs * num)

    getResult keypads1, getResult keypads2
