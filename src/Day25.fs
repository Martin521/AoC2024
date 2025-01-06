module Day25

open AoClib

let testInput =
    """#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"""

let getResults (lines: string list, example) =
    let lines = if example = "1" then splitLines testInput else lines
    let blocks = lines |> List.map Seq.toList |> List.chunkBySize 8 |> List.map (List.take 7)
    let lockBlocks, keyBlocks = blocks |> List.partition (fun block -> block.Head.Head = '#')
    let getHeight column = column |> List.takeWhile (fun c -> c = '#') |> List.length
    let locks = lockBlocks |> List.map (List.tail >> List.transpose >> List.map getHeight)
    let keys = keyBlocks |> List.map (List.rev >> List.tail >> List.transpose >> List.map getHeight)
    
    let fit (key, lock) = List.zip key lock |> List.forall (fun (k, l) -> k + l <= 5)
    let result1 = List.allPairs keys locks |> List.filter fit |> List.length

    result1, 0
