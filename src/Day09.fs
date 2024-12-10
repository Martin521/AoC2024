module Day09

open AoClib

let testInput = """2333133121414131402"""

let getResult1 blockCountList =
    let blockCounts = blockCountList |> List.toArray
    let rec check i j k checkSum =
        if i % 2 = 0 then
            if blockCounts[i] = 0 then
                check (i + 1) j k checkSum
            else
                blockCounts[i] <- blockCounts[i] - 1
                check i j (k + 1) (bigint (k * i / 2) + checkSum)
        else if blockCounts[i] = 0 then
            check (i + 1) j k checkSum
        elif blockCounts[j] = 0 then
            if i >= j - 2 then checkSum else check i (j - 2) k checkSum
        else
            blockCounts[i] <- blockCounts[i] - 1
            blockCounts[j] <- blockCounts[j] - 1
            check i j (k + 1) (bigint (k * j / 2) + checkSum)
    check 0 (blockCounts.Length - 1) 0 0I

type File = {fIndex: int; blocks: int; hasMoved: bool}
type Space = {sIndex: int; movedFiles: File list; remaining: int}
[<RequireQualifiedAccessAttribute>]
type Check =
    | File
    | Space
type State = {files: File array; spaces: Space array; index: int; check: Check; blockPosition: int}

let print = false

[<TailCallAttribute>]
let rec check state checkSum =
    let i = state.index
    let k = state.blockPosition
    let mkState c i k = {state with check = c; index = i; blockPosition = k}
    if i >= state.spaces.Length then
        checkSum
    else

    match state.check with
    | Check.Space ->
        let {sIndex = sIndex; movedFiles = movedFiles; remaining = remaining} as space = state.spaces[i]
        assert (sIndex = i)
        match movedFiles, remaining with
        | [], 0 -> check (mkState Check.File i k) checkSum
        | h :: t, _ ->
            if h.blocks = 0 then
                state.spaces[i] <- {space with movedFiles = t}
                check (mkState Check.Space i k) checkSum
            else
                state.spaces[i] <- {space with movedFiles = {h with blocks = h.blocks - 1} :: t}
                if print then
                    printfn $"{i}s - {h.fIndex}: {k}*{h.fIndex}={k * h.fIndex} => {bigint (k * h.fIndex) + checkSum}"
                check (mkState Check.Space i (k + 1)) (bigint (k * h.fIndex) + checkSum)
        | [], _ ->
            state.spaces[sIndex] <- {space with remaining = space.remaining - 1}
            if print then printfn $"{i}s: {k} (space) => {checkSum}"
            check (mkState Check.Space i (k + 1)) checkSum
    | Check.File ->
        let {fIndex = fIndex; blocks = blocks; hasMoved = hasMoved} as file = state.files[i]
        assert (fIndex = i + 1)
        match blocks, hasMoved with
        | 0, _ -> check (mkState Check.Space (i + 1) k) checkSum
        | _, false ->
            state.files[i] <- {file with blocks = file.blocks - 1}
            if print then
                printfn $"{i}f: {k}*{fIndex}={k * fIndex} => {bigint (k * fIndex) + checkSum}"
            check (mkState Check.File i (k + 1)) (bigint (k * fIndex) + checkSum)
        | _, true ->
            state.files[i] <- {file with blocks = file.blocks - 1}
            if print then printfn $"{i}f: {k} (moved) => {checkSum}"
            check (mkState Check.File i (k + 1)) checkSum

let getResult2 blockCountList =
    let blockCounts = blockCountList |> List.toArray
    let initialState =
        let mkFile (i, n) = {fIndex = i + 1; blocks = n; hasMoved = false}
        let mkSpace (i, n) = {sIndex = i; movedFiles = []; remaining = n}
        let files =
            blockCounts
            |> Array.indexed
            |> Array.filter (fun (i, c) -> i % 2 = 0)
            |> Array.tail
            |> Array.map snd
            |> Array.indexed
            |> Array.map mkFile
        let spaces =
            blockCounts
            |> Array.indexed
            |> Array.filter (fun (i, _) -> i % 2 = 1)
            |> Array.map snd
            |> Array.indexed
            |> Array.map mkSpace
        {files = files; spaces = spaces; index = 0; check = Check.Space; blockPosition = blockCounts[0]}
    let tryMoveFile state file =
        match state.spaces[.. file.fIndex - 1] |> Array.tryFind (fun space -> space.remaining >= file.blocks) with
        | None -> state
        | Some space ->
            state.files[file.fIndex - 1] <- {file with hasMoved = true}
            state.spaces[space.sIndex] <-
                {space with remaining = space.remaining - file.blocks; movedFiles = space.movedFiles @ [file]}
            state
    let state = (initialState, Array.rev initialState.files) ||> Array.fold tryMoveFile
    check state 0I

let getResults (lines: string list) =
    let lines = if useExample then splitLines testInput else lines
    let blockCountList = List.exactlyOne lines |> Seq.map (fun c -> int (c - '0')) |> Seq.toList
    getResult1 blockCountList, getResult2 blockCountList
