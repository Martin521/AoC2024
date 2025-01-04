module Day23

open AoClib
open Utilities.Combinatorics

let testInput =
    """kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"""

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput else lines
    let edges = lines |> List.map (splitLine "-" >> List.pairwise >> List.exactlyOne)
    let dedges = edges @ (edges |> List.map (fun (a, b) -> b, a))
    let neighbors = dedges |> List.groupBy fst |> Map |> Map.map (fun _ -> List.map snd >> Set)
    let nextdoor a b = neighbors[a].Contains b
    let nodes = neighbors.Keys |> Seq.toList
    let getTriplesWith n =
        neighbors[n]
        |> Set.toList
        |> Combinations 2
        |> List.filter (fun pair -> nextdoor pair[0] pair[1])
        |> List.map (fun p -> Set(n :: p))
    let result1 = nodes |> List.filter (startsWith "t") |> List.collect getTriplesWith |> Set |> Set.count

    let rec bk n r p x =  // Bron-Kerbosch
        match p with
        | [] -> if List.isEmpty x then [r] else []
        | h :: t -> bk n (h :: r) (List.filter (n h) p) (List.filter (n h) x) @ bk n r t (h :: x)
    let result2 = bk nextdoor [] nodes [] |> List.maxBy List.length |> List.sort |> String.concat ","

    string result1, result2
