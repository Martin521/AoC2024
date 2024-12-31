module Day04

open AoClib

let testInput =
    """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput else lines
    let nr = lines.Length
    let nc = lines.Head.Length
    let chars = lines |> List.map Seq.toList |> array2D

    let dirs = [1, 0; -1, 0; 0, 1; 0, -1; 1, 1; 1, -1; -1, 1; -1, -1]
    let rec hasWord (word: string) (r, c) (dr, dc) =
        if word.Length = 0 then true
        elif r < 0 || r >= nr || c < 0 || c >= nr then false
        elif word[0] <> chars[r, c] then false
        else hasWord word[1..] (r + dr, c + dc) (dr, dc)
    let countWordStartingAt word pos = dirs |> List.sumBy (fun d -> if hasWord word pos d then 1 else 0)
    let result1 = allPositions nr nc |> List.sumBy (countWordStartingAt "XMAS")

    let MasCount (r, c) =
        if chars[r, c] <> 'A' then 0
        else
            let se = chars[r + 1, c + 1]
            let nw = chars[r - 1, c - 1]
            let sw = chars[r + 1, c - 1]
            let ne = chars[r - 1, c + 1]
            if (se = 'M' && nw = 'S' || se = 'S' && nw = 'M')
                && (ne = 'M' && sw = 'S' || ne = 'S' && sw = 'M') then 1 else 0
    let result2 = allInnerPositions nr nc |> List.sumBy MasCount

    result1, result2
