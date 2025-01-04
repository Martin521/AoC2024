module Day22

open AoClib

let testInput =
    """1
2
3
2024"""

let purgeMask = (1UL <<< 24) - 1UL

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput else lines
    let seeds = lines |> List.map (System.UInt64.Parse)

    let sgen secret =
        let secret = ((secret <<< 6) ^^^ secret) &&& purgeMask
        let secret = ((secret >>> 5) ^^^ secret) &&& purgeMask
        let secret = ((secret <<< 11) ^^^ secret) &&& purgeMask
        secret
    let rec nth n seed = match n with 0 -> seed | _ -> nth (n - 1) (sgen seed)
    let result1 = seeds |> List.sumBy (nth 2000) |> int64

    let getResult2() =
        let getOpportunities seed =
            seed
            |> List.replicate 2000
            |> List.scan (fun secret _ -> sgen secret) seed
            |> List.map (fun s -> int s % 10)
            |> List.windowed 5
            |> List.map (fun w -> w |> List.pairwise |> List.map (fun (a, b) -> b - a), List.last w)
        let opportunities = seeds |> List.map getOpportunities
        let oppMaps = opportunities |> List.map (List.rev >> Map)
        let changeSequences = opportunities |> List.collect (List.map fst) |> Set |> Set.toList
        let bananaCount cseq = oppMaps |> List.sumBy (Map.tryFind cseq >> Option.defaultValue 0)
        changeSequences |> List.maxBy bananaCount |> bananaCount |> int64

    let getResult2Faster() =
        let rec getSecrets n seed = match n with 0 -> [seed] | _ -> seed :: getSecrets (n - 1) (sgen seed)
        let maxRawIndex = 18*19*19*19
        let size = 2 * maxRawIndex
        let getIndex (ds: int list) =
            (((ds[1] - ds[0]) * 19 + ds[2] - ds[1]) * 19 + ds[3] - ds[2]) * 19 + ds[4] - ds[3] + maxRawIndex
        let bananas = Array.create size 0
        let negotiate seed =
            let seen = System.Collections.Generic.HashSet()
            let processWindow w =
                let index = getIndex w
                let price = List.last w
                if not <| seen.Contains index then
                    seen.Add(index) |> ignore
                    bananas[index] <- bananas[index] + price
            seed
            |> getSecrets 2000
            |> List.map (fun s -> int s % 10)
            |> List.windowed 5
            |> List.iter processWindow
        seeds |> List.iter negotiate
        Array.max bananas |> int64

    result1, getResult2Faster()
