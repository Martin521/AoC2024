module Day07

open AoClib

let testInput =
    """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

let getResults lines =
    let lines = if useExample then splitLines testInput else lines
    let getEquation (line: string) =
        let split1 = line.Split(":")
        let split2 = split1[1].Trim().Split(" ") |> Array.map int64
        int64 split1[0], Array.toList split2
    let equations = lines |> List.map getEquation
    let ops1 = [(+); (*)]
    let isCorrect ops (expected, (nums: _ list)) =
        let rec check res nums =
            match nums with
            | [] -> res = expected
            | h::t -> ops |> List.exists (fun op -> check (op res h) t)
        check nums.Head nums.Tail
    let result ops = equations |> List.filter (isCorrect ops) |> List.sumBy fst

    let rec shift a = function 0L -> a | b -> shift (a * 10L) (b / 10L)
    let concat a b = shift a b + b
    let ops2 = concat :: ops1

    result ops1, result ops2
