module AoClib

open System

let mutable useExample = false

let splitLines (testInput: string) =
    testInput.ReplaceLineEndings(Environment.NewLine).Split(Environment.NewLine) |> Array.toList

let splitLine (separator: string) (line: string) =
    line.Split separator |> Array.toList

let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd
