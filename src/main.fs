module AoC

open System
open System.IO
open System.Reflection

let private expectedResultsFileName = "results.txt"
let private inputFileName day = $"input/%s{day}.txt"
let private resultFuncName = "getResults"

let private computeResults lines day additionalArg =
    match Assembly.GetExecutingAssembly().ExportedTypes |> Seq.tryFind (fun ty -> ty.Name = day) with
    | None -> Error $"no module {day}"
    | Some dayType ->
        match dayType.GetMethod(resultFuncName) with
        | null -> Error $"missing function '{resultFuncName}' in module '{day}'"
        | method ->
            match method.Invoke(null, [|lines; additionalArg|]) |> nonNull with
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

let private test additionalArg (day, (e1, e2)) =
    let inputFile = $"input/%s{day}.txt"
    if not <| File.Exists inputFile then
        Error $"no input file {inputFile}"
    else
        let lines = File.ReadAllLines inputFile |> Array.toList
        let start = DateTime.Now
        match computeResults lines day additionalArg with
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
    let progName = Path.GetFileNameWithoutExtension args[0]
    if args.Length = 1 then
        printfn $"""Call this program with the day as argument ("{progName} Day01")."""
        printfn $"""Or with an additional arg that will be threaded into the day code ("{progName} Day01 1")."""
        printfn $"""Or "{progName} all" to run all days."""
    elif args.Length > 1 then
        let day = args[1]
        let additionalArg = if args.Length > 2 then args[2] else ""
        if day = "all" then
            results |> Map.toList |> List.iter ((test additionalArg) >> showResult false)
        else
            match results.TryFind day with
            | None -> printfn $"'{day}' is not a valid day"
            | Some result -> test additionalArg (day, result) |> showResult true
