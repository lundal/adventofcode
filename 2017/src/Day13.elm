module Day13 exposing (main)

import Answer
import Dict exposing (Dict)


type alias Scanner =
    { depth : Int, range : Int }


isCaughtAt delay scanner =
    (delay + scanner.depth) % ((scanner.range - 1) * 2) == 0


severity scanner =
    scanner.range * scanner.depth



-- Part 1


severityAt delay scanners =
    scanners
        |> List.filter (isCaughtAt delay)
        |> List.map severity
        |> List.sum



-- Part 2


canPassAt delay scanners =
    scanners
        |> List.filter (isCaughtAt delay)
        |> List.isEmpty


findMinDelay delay scanners =
    if canPassAt delay scanners then
        delay
    else
        findMinDelay (delay + 1) scanners



-- Main


input =
    "0: 3\n1: 2\n2: 4\n4: 4\n6: 5\n8: 8\n10: 6\n12: 6\n14: 6\n16: 6\n18: 8\n20: 8\n22: 12\n24: 10\n26: 9\n28: 8\n30: 8\n32: 12\n34: 12\n36: 12\n38: 12\n40: 8\n42: 12\n44: 14\n46: 14\n48: 10\n50: 12\n52: 12\n54: 14\n56: 14\n58: 14\n62: 12\n64: 14\n66: 14\n68: 14\n70: 12\n74: 14\n76: 14\n78: 14\n80: 18\n82: 17\n84: 30\n88: 14"


parseScanners input =
    input
        |> String.lines
        |> List.map parseScanner


parseScanner input =
    let
        scanner =
            input
                |> String.split ": "
                |> List.map String.toInt
                |> List.map (Result.withDefault 0)
    in
        case scanner of
            [ depth, range ] ->
                Scanner depth range

            _ ->
                Debug.crash "Wat!?"


part1 =
    input
        |> parseScanners
        |> severityAt 0


part2 =
    input
        |> parseScanners
        |> findMinDelay 0


main =
    Answer.render part1 part2
