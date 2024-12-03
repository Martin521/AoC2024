module Day02

open AoClib

let testInput =
    """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""

let getResults (lines: string list) =
    // let lines = splitLines testInput
    let reports = lines |> List.map (splitLine " " >> List.map int)
    let isSafe report =
        let diffs = report |> List.pairwise |> List.map (fun (a, b) -> b - a)
        List.forall (fun d -> d >= 1 && d <= 3) diffs || List.forall (fun d -> d <= -1 && d >= -3) diffs
    let result1 = reports |> List.filter isSafe |> List.length

    let dReports report = report |> List.mapi (fun i _ -> List.removeAt i report)
    let isSafe2 report = report :: dReports report |> List.exists isSafe
    let result2 = reports |> List.filter isSafe2 |> List.length

    result1, result2
