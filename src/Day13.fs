module Day13

open AoClib
open System.Text.RegularExpressions

#nowarn 25

let testInput =
    """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"""

let getResults (lines: string list) =
    let lines = if useExample then splitLines testInput else lines
    let regex = Regex(""".+: X[+=](\d+), Y[+=](\d+)""")
    let getEquation line = regex.Match(line).Groups |> Seq.tail |> Seq.map (_.Value >> int >> bigint) |> Seq.toList
    let machines = lines |> List.chunkBySize 4 |> List.map (List.take 3 >> List.map getEquation >> List.transpose)

    let getResult isPart2 =
        let solve equations =
            let [x1; x2; X] = List.head equations
            let [y1; y2; Y] = equations.Tail.Head
            let X, Y = if isPart2 then X + 10000000000000I, Y + 10000000000000I else X, Y
            let nom2 = X * y1 - Y * x1
            let den2 = x2 * y1 - y2 * x1
            let nom1 = X * y2 - Y * x2
            let den1 = - den2
            if nom1 % den1 = 0I && nom2 % den2 = 0I then Some (nom1 / den1, nom2 / den2) else None
        machines |> List.choose solve |> List.sumBy (fun (n1, n2) -> 3I * n1 + n2)

    getResult false, getResult true

