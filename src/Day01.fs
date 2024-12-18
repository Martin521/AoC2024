module Day01

open AoClib

let testInput =
    """3   4
4   3
2   5
1   3
3   9
3   3"""

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput else lines
    let ids = lines |> List.map (splitLine "   " >> List.map int)
    let ids1 = ids |> List.map List.head |> List.sort
    let ids2 = ids |> List.map (List.tail >> List.head) |> List.sort
    let result1 = List.zip ids1 ids2 |> List.sumBy (fun (id1, id2) -> abs (id1 - id2))

    let occurencies = ids2 |> List.countBy id |> Map
    let score i =
        match occurencies.TryFind i with
        | Some n -> i * n
        | None -> 0
    let result2 = ids1 |> List.sumBy score

    result1, result2
