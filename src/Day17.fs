module Day17

open AoClib

#nowarn 25

let testInput1 =
    """Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"""

let testInput2 = """Register A: 14680
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
"""

let getResults (lines: string list, example) =
    let lines = match example with "1" -> splitLines testInput1 | "2" -> splitLines testInput2 | _ -> lines
    let oRegs = lines |> List.take 3 |> List.map (fun line -> (line.Split(":")[1]).Trim() |> int64) |> List.toArray
    let prog = ((lines |> List.skip 4 |> List.head).Split(" ")[1]).Split(",") |> Seq.map int |> Seq.toArray

    let regs = Array.copy oRegs
    let rec step out ip =
        if ip + 1 >= prog.Length then out
        else

        let operand = prog[ip + 1]
        let inline combo() = if operand < 4 then int64 operand else regs[operand - 4]
        let inline rightShiftByCombo reg =
            let c = combo()
            if c < 64 then reg >>> int c else 0L
        match prog[ip] with
        | 0 ->
            regs[0] <- rightShiftByCombo regs[0]
            step out (ip + 2)
        | 1 ->
            regs[1] <- regs[1] ^^^ operand
            step out (ip + 2)
        | 2 ->
            regs[1] <- combo() % 8L
            step out (ip + 2)
        | 3 ->
            step out (if regs[0] = 0 then ip + 2 else operand)
        | 4 ->
            regs[1] <- regs[1] ^^^ regs[2]
            step out (ip + 2)
        | 5 ->
            step (int (combo() % 8L) :: out) (ip + 2)
        | 6 ->
            regs[1] <- rightShiftByCombo regs[0]
            step out (ip + 2)
        | 7 ->
            regs[2] <- rightShiftByCombo regs[0]
            step out (ip + 2)
    let result1 = step [] 0 |> List.rev |> List.map string |> String.concat ","

    let run a =
        regs[0] <- a
        regs[1] <- oRegs[1]
        regs[2] <- oRegs[2]
        step [] 0
    let revProg = prog |> Array.toList |> List.rev
    let rec findSelf n aOptions =
        if n > revProg.Length then
            aOptions
        else
            let newOptions a = [for i in 0L..7L -> (a <<< 3) + i]
            findSelf (n + 1) (aOptions |> List.collect newOptions |> List.filter (run >> (=) (List.take n revProg)))
    let result2 = findSelf 1 [0L] |> List.min

    result1, string result2
