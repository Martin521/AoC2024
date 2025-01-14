module Day03

open System.Text.RegularExpressions

let getResults (lines: string list, example: string) =

    let input = String.concat "" lines

    let getMulSum s =
        Regex.Matches(s, """mul\((\d+),(\d+)\)""")
        |> Seq.sumBy (fun m -> int m.Groups[1].Value * int m.Groups[2].Value)
    let result1 = getMulSum input

    let rec getEnabledSections (s: string) =
        let skipDisabledSection (s: string) =
            match s.IndexOf "do()" with
            | -1 -> []
            | i -> getEnabledSections s[i + 4 ..]
        match s.IndexOf "don't()" with
        | -1 -> [s]
        | i -> s[..i] :: skipDisabledSection s[i + 7 ..]
    let result2 = getEnabledSections input |> List.sumBy getMulSum

    result1, result2
