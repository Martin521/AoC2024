module AoC

open System
open System.IO
open System.Reflection

let expectedResultsFileName = "results.txt"
let inputFileName day = $"input/%s{day}.txt"
let resultFuncName = "getResults"

let private computeResults lines day =
    match Assembly.GetExecutingAssembly().ExportedTypes |> Seq.tryFind (fun ty -> ty.Name = day) with
    | None -> "ERROR", $"no module {day}"
    | Some dayType ->
        match dayType.GetMethod(resultFuncName) with
        | null -> $"missing function '{resultFuncName}' in module '{day}'", ""
        | method ->
            match method.Invoke(null, [|lines|]) |> nonNull with
            | :? (string * string) as (r1, r2) -> r1, r2
            | :? (int * int) as (r1, r2) -> string r1, string r2
            | :? (int64 * int64) as (r1, r2) -> string r1, string r2
            | :? (bigint * bigint) as (r1, r2) -> string r1, string r2
            | r -> $"not a valid '{resultFuncName}' output type: {r.GetType()}", ""

let private readExpectedResults fileName =
    File.ReadAllLines(fileName)
    |> Array.toList
    |> List.map (fun s -> s.Split " ")
    |> List.filter (fun words -> words.Length = 3)
    |> List.map (fun words -> words[0], (words[1], words[2]))
    |> Map

let private test day (e1, e2) =
    let inputFile = $"input/%s{day}.txt"
    if not <| File.Exists inputFile then
        printfn $"no input file {inputFile}"
    else
        let lines = File.ReadAllLines inputFile |> Array.toList
        let start = DateTime.Now
        let r1, r2 = computeResults lines day
        let duration = (DateTime.Now - start).TotalMilliseconds
        if e1 = r1 && e2 = r2 then
            printfn $"{day} ok ({duration} ms)"
        else
            printfn $"{day}: expected {e1}; {e2}, got {r1}, {r2} ({duration} ms)"

let args = Environment.GetCommandLineArgs()
if not <| File.Exists expectedResultsFileName then
    printfn $"{expectedResultsFileName} not found"
else
    let results = readExpectedResults expectedResultsFileName
    if args.Length > 2 then
        printfn $"Call this program with just the day number as argument (or without arg)"
    elif args.Length = 2 then
        let day = args[1]
        match results.TryFind day with
        | None -> printfn $"'{day}' is not a valid day"
        | Some result -> test day result
    else
        Map.iter test results
