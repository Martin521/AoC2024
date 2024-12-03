module Day03

open System.Text.RegularExpressions

let getResults (lines: string list) =

    let input = String.concat "" lines

    let getMulSum s =
        Regex.Matches(s, "mul\((\d+),(\d+)\)")
        |> Seq.sumBy (fun m -> int m.Groups[1].Captures[0].Value * int m.Groups[2].Captures[0].Value)
    let result1 = getMulSum input

    let dontIndices = Regex.Matches(input, "(don't)") |> Seq.map _.Index |> Seq.toList
    let dontSet = Set dontIndices
    let doIndices =
        Regex.Matches(input, "(do)") |> Seq.map _.Index |> Seq.filter (dontSet.Contains >> not) |> Seq.toList
    let rec getSectionsStarted start dos donts =
        match donts with
        | [] -> [start, input.Length - 1]
        | h :: t when h < start -> getSectionsStarted start dos t
        | h :: t -> (start, donts.Head) :: getSections h dos t
    and getSections prevEnd dos donts =
        match dos with
        | [] -> []
        | h :: t when h < prevEnd -> getSections prevEnd t donts
        | h :: t -> getSectionsStarted h t donts
    let enabledSections = getSectionsStarted 0 doIndices dontIndices |> List.map (fun (s, e) -> input[s..e])
    let result2 = enabledSections |> List.sumBy getMulSum

    result1, result2
