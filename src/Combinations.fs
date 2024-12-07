module Utilities.Combinatorics

let rec Selections =
    function
    | x :: xs -> (x, xs) :: (Selections xs |> List.map (fun (y, ys) -> y, x :: ys))
    | _ -> []

let rec PairSelection =
    function
    | [p1; p2] -> [p1, p2, []]
    | x :: xs ->
        (Selections xs |> List.map (fun (y, ys) -> x, y, ys))
        @ (PairSelection xs |> List.map (fun (y1, y2, ys) -> y1, y2, x :: ys))
    | [] -> []

//let rec KPermutationsO k lst =
//  (k, lst) |> function
//  | 0, _ -> [[]]
//  | _, [] -> []
//  | _, x::[] -> [[x]]
//  | k, lst -> Selections lst |> List.collect (fun (z,r) -> (KPermutationsO (k-1) r |> List.map (fun zs -> z::zs)))

let rec Combinations n lst =
    match (n, lst) with
    | 0, _ -> [[]]
    | _, h :: t -> List.fold (fun cs c -> (h :: c) :: cs) (Combinations n t) (Combinations (n - 1) t)
    | _ -> []

let rec private insertions x =
    function
    | (y :: ys) as l -> (x :: l) :: (List.map (fun x -> y :: x) (insertions x ys))
    | _ -> [[x]]

let rec Permutations =
    function
    | x :: xs -> List.collect (insertions x) (Permutations xs)
    | _ -> [[]]

let KPermutations k lst = Combinations k lst |> List.collect Permutations

let rec AllSubsets =
    function
    | [] -> [[]]
    | h :: t -> AllSubsets t |> List.collect (fun ss -> [h :: ss; ss])

let Cartesian xss =
    let rec add x yss s =
        match yss with
        | [] -> s
        | ys :: yss' -> add x yss' ((x :: ys) :: s)
    let rec mul xs yss p =
        match xs with
        | [] -> p
        | x :: xs' -> mul xs' yss (add x yss p)
    let rec cartesian xss c =
        match xss with
        | [] -> c
        | xs :: xss' -> cartesian xss' (mul xs c [])
    cartesian xss [[]]

let rec allPairs = function [] -> [] | h::t -> List.fold (fun pairs x -> (h, x)::pairs) (allPairs t) t

