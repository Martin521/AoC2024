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

let getResults (lines: string list) =
    // let lines = splitLines testInput
    let nr = lines.Length
    let nc = lines.Head.Length
    let chars = lines |> List.map Seq.toList |> array2D

    let dirs = [1, 0; -1, 0; 0, 1; 0, -1; 1, 1; 1, -1; -1, 1; -1, -1]
    let rec hasWord (word: string) (dr, dc) (r, c) =
        if word.Length = 0 then true
        elif r < 0 || r >= nr || c < 0 || c >= nr then false
        elif word[0] <> chars[r, c] then false
        else hasWord word[1..] (dr, dc) (r + dr, c + dc)
    let mutable result1 = 0
    for r in 0 .. nr - 1 do
        for c in 0 .. nc - 1 do
            for d in dirs do
                if hasWord "XMAS" d (r, c) then
                    result1 <- result1 + 1

    let dirs = [1, 1; 1, -1; -1, 1; -1, -1]
    let hasMAS (d1, d2) (r, c) =
        chars[r, c] = 'A'
        && chars[r + d1, c + d1] = 'M'
        && chars[r - d1, c - d1] = 'S'
        && chars[r + d2, c - d2] = 'M'
        && chars[r - d2, c + d2] = 'S'
    let mutable result2 = 0
    for r in 1 .. nr - 2 do
        for c in 1 .. nc - 2 do
            for d in dirs do
                if hasMAS d (r, c) then
                    result2 <- result2 + 1

    result1, result2
