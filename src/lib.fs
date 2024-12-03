module AoClib

open System

let splitLines (testInput: string) =
    testInput.ReplaceLineEndings(Environment.NewLine).Split(Environment.NewLine) |> Array.toList

let splitLine (separator: string) (line: string) =
    line.Split separator |> Array.toList

