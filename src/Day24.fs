module Day24

open AoClib

let testInput =
    """x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02"""

let testInput2 = """x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj"""

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput elif example = "2" then splitLines testInput2 else lines
    let inputs =
        lines
        |> List.takeWhile (strLen >> (<>) 0)
        |> List.map (splitLine ":" >> fun a -> a.Head, System.Int32.Parse a.Tail.Head)
        |> Map
    let gatesList =
        lines
        |> List.skipWhile (strLen >> (<>) 0)
        |> List.tail
        |> List.map (splitLine " " >> fun a -> a[4], (a[1], (a[0], a[2])))
    let gates = Map gatesList
    let outputs = gates |> Map.filter (fun out _ -> startsWith "z" out)

    // Part 1

    let rec getState wire =
        let state =
            if inputs.ContainsKey wire then
                inputs[wire]
            elif gates.ContainsKey wire then
                let gateType, (input1, input2) = gates[wire]
                let state1, state2 = getState input1, getState input2
                match gateType with
                | "AND" -> (state1 + state2) / 2
                | "OR" -> (state1 + state2 + 1) / 2
                | "XOR" -> if state1 <> state2 then 1 else 0
                | _ -> failwith "unknown gate type"
            else failwith "unknown wire"
        state
    
    let rec fromBinary = function [] -> 0L | h::t -> int64 h + 2L * fromBinary t
    
    let result1 =
        [0 .. outputs.Count - 1]
        |> List.map (fun i -> getState $"z%02d{i}")
        |> fromBinary

    // Part 2

    let sInputs = Set [for j in 1 .. inputs.Count / 2 - 1 do for i in ["x"; "y"] do yield $"{i}%02d{j}"]
    assert (sInputs.Add("x00").Add("y00") = Set inputs.Keys)

    let getTargets gateType wire =
        gatesList |> List.filter (fun (_, (gt, (in1, in2))) -> (in1 = wire || in2 = wire) && gt = gateType)

    let getAllTargets wire = getTargets "XOR" wire, getTargets "AND" wire, getTargets "OR" wire

    let haGates wire =
        let xList, aList, oList = getAllTargets wire
        if xList.Length = 1 && aList.Length = 1 && oList.IsEmpty then
            Some (xList |> List.exactlyOne |> fst, aList |> List.exactlyOne |> fst)
        else
            None

    let checkHalfadderInputs i input1 input2 =
        match haGates input1, haGates input2 with
        | Some gs1, Some gs2 when gs1 = gs2 -> gs1, None
        | Some gs1, None -> gs1, Some input2
        | None, Some gs2 -> gs2, Some input1
        | _ -> failwith $"unexpeced halfadder config in {i}"

    let checkOutput i wire =
        let errCond = wire <> $"z%02d{i}" || List.exists (fun (_, (_, (in1, in2))) -> in1 = wire || in2 = wire) gatesList
        if errCond then Some wire else None

    let ccGate wire =
        let xList, aList, oList = getAllTargets wire
        if xList.IsEmpty && aList.IsEmpty && oList.Length = 1 then
            Some (oList |> List.exactlyOne |> fst)
        else
            None

    let checkCarryCombi i ha1carry ha2carry =
        match ccGate ha1carry, ccGate ha2carry with
        | Some g1, Some g2 when g1 = g2 -> g1, None
        | Some g1, None -> g1, Some ha2carry
        | None, Some g2 -> g2, Some ha1carry
        | _ -> failwith $"unexpected carry combinator in {i}"

    let checkAdder i carry =
        let (ha1out, ha1carry), ha1error = checkHalfadderInputs i $"x%02d{i}" $"y%02d{i}"
        let (ha2out, ha2carry), ha2error = checkHalfadderInputs i carry ha1out
        let outError = checkOutput i ha2out
        let outCarry, ccError = checkCarryCombi i ha1carry ha2carry
        let errors = List.choose id [ha1error; ha2error; outError; ccError]
        outCarry, errors

    let carry0 =
        let (out0, carry0), error0 = checkHalfadderInputs 0 "x00" "y00"
        if error0.IsSome || out0 <> "z00" then failwith "unexpected bit 0 half adder config"
        carry0
    
    let rec collectErrors i carry =
        if i = inputs.Count / 2 then
            if carry <> $"z%02d{i}" then failwith "unexpeced MSB output"
            []
        else
            let outCarry, errors = checkAdder i carry
            errors @ collectErrors (i + 1) outCarry
    
    let result2 = collectErrors 1 carry0 |> List.sort |> String.concat ","

    string result1, result2
