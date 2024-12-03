module Day03

open System.Text.RegularExpressions

let getResults (lines: string list) =

    let input = String.concat "" lines

    let getMulSum s =
        Regex.Matches(s, "mul\((\d+),(\d+)\)")
        |> Seq.sumBy (fun m -> int m.Groups[1].Captures[0].Value * int m.Groups[2].Captures[0].Value)
    let result1 = getMulSum input

    let rec getEnabledSections (s: string) =
        match s.IndexOf "don't()" with
        | -1 -> [s]
        | i -> s[..i] :: getEnabledSectionsNext s[i + 7 ..]
    and getEnabledSectionsNext s =
        match s.IndexOf "do()" with
        | -1 -> []
        | i -> getEnabledSections s[i..]
    let result2 = getEnabledSections input |> List.sumBy getMulSum

    result1, result2
