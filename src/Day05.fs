module Day05

open AoClib

let testInput =
    """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""

let getResults (lines: string list) =
    // let lines = splitLines testInput
    let orders =
        lines
        |> List.takeWhile (fun l -> l.Length > 0)
        |> List.map (fun l -> l.Split("|"))
        |> List.map (fun p -> int p[0], int p[1])
        |> Set
    let prints =
        lines
        |> List.skipWhile (fun l -> l.Length > 0)
        |> List.tail
        |> List.map (fun l -> l.Split(",") |> Array.map int)

    let isOrdered print = print |> Array.pairwise |> Array.forall orders.Contains
    let result1 = prints |> List.sumBy (fun p -> if isOrdered p then p[p.Length / 2] else 0)

    let ordered print = print |> Array.sortWith (fun p1 p2 -> if orders.Contains(p1, p2) then -1 else 1)
    let result2 = prints |> List.sumBy (fun p -> if isOrdered p then 0 else (ordered p)[p.Length / 2])

    result1, result2
