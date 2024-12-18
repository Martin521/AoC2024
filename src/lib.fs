module AoClib

open System

let splitLines (testInput: string) =
    testInput.ReplaceLineEndings(Environment.NewLine).Split(Environment.NewLine) |> Array.toList

let splitLine (separator: string) (line: string) =
    line.Split separator |> Array.toList

let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

let rec gcd x y = if y = 0L then x else gcd y (x % y)
let lcm x y = x * y / gcd x y

