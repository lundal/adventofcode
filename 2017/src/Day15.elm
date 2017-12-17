module Day15 exposing (main)

import Answer


type alias Generator =
    { factor : Int, modulo : Int, requiredModulo : Int, value : Int }


generate generator =
    let
        nextValue =
            (generator.value * generator.factor) % generator.modulo

        nextGenerator =
            { generator | value = nextValue }
    in
        if nextValue % generator.requiredModulo == 0 then
            nextGenerator
        else
            generate nextGenerator


judge genA genB iterations matches =
    if iterations == 0 then
        matches
    else
        let
            nextGenA =
                generate genA

            nextGenB =
                generate genB

            nextIterations =
                iterations - 1

            nextMatches =
                if nextGenA.value % 65536 == nextGenB.value % 65536 then
                    matches + 1
                else
                    matches
        in
            judge nextGenA nextGenB nextIterations nextMatches



-- Main


part1 =
    judge
        { factor = 16807, modulo = 2147483647, requiredModulo = 1, value = 289 }
        { factor = 48271, modulo = 2147483647, requiredModulo = 1, value = 629 }
        40000000
        0


part2 =
    judge
        { factor = 16807, modulo = 2147483647, requiredModulo = 4, value = 289 }
        { factor = 48271, modulo = 2147483647, requiredModulo = 8, value = 629 }
        5000000
        0


main =
    Answer.render part1 part2
