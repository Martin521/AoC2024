module AoC

open System
open System.IO
open System.Reflection

let private expectedResultsFileName = "results.txt"
let private inputFileName day = $"input/%s{day}.txt"
let private resultFuncName = "getResults"

let private computeResults lines day =
    match Assembly.GetExecutingAssembly().ExportedTypes |> Seq.tryFind (fun ty -> ty.Name = day) with
    | None -> Error $"no module {day}"
    | Some dayType ->
        match dayType.GetMethod(resultFuncName) with
        | null -> Error $"missing function '{resultFuncName}' in module '{day}'"
        | method ->
            match method.Invoke(null, [|lines|]) |> nonNull with
            | :? (string * string) as (r1, r2) -> Ok (r1, r2)
            | :? (int * int) as (r1, r2) -> Ok (string r1, string r2)
            | :? (int64 * int64) as (r1, r2) -> Ok (string r1, string r2)
            | :? (bigint * bigint) as (r1, r2) -> Ok (string r1, string r2)
            | r -> Error $"not a valid '{resultFuncName}' output type: {r.GetType()}"

let private readExpectedResults fileName =
    File.ReadAllLines(fileName)
    |> Array.toList
    |> List.map (fun s -> s.Split " ")
    |> List.filter (fun words -> words.Length = 3)
    |> List.map (fun words -> words[0], (words[1], words[2]))
    |> Map

let private test (day, (e1, e2)) =
    let inputFile = $"input/%s{day}.txt"
    if not <| File.Exists inputFile then
        Error $"no input file {inputFile}"
    else
        let lines = File.ReadAllLines inputFile |> Array.toList
        let start = DateTime.Now
        match computeResults lines day with
        | Ok (r1, r2) ->
            let duration = (DateTime.Now - start).TotalMilliseconds
            if e1 = r1 && e2 = r2 then
                Ok $"{day} ok ({duration} ms)"
            else
                Ok $"{day}: expected {e1}; {e2}, got {r1}, {r2} ({duration} ms)"
        | Error e -> Error e
    
let showResult verbose result =
    match result with
    | Error e -> if verbose then printfn $"%s{e}"
    | Ok m -> printfn $"%s{m}"


let args = Environment.GetCommandLineArgs()
if not <| File.Exists expectedResultsFileName then
    printfn $"{expectedResultsFileName} not found"
else
    let results = readExpectedResults expectedResultsFileName
    if args.Length > 2 then
        printfn """Call this program with the day as argument (like "Day01", or "xDay01" to use the example data). Or without arg for all tests"""
    elif args.Length = 2 then
        let day = args[1]
        let day, useEx = if day.StartsWith "x" then day[1..], true else day, false
        AoClib.useExample <- useEx
        match results.TryFind day with
        | None -> printfn $"'{day}' is not a valid day"
        | Some result -> test (day, result) |> showResult true
    else
        results |> Map.toList |> List.iter (test >> showResult false)
