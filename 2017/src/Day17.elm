module Day17 exposing (main)

import Answer


--Part 1


spin : Int -> Int -> Int -> Int -> List Int -> List Int
spin step inserts currentPosition currentValue list =
    let
        nextPos =
            (currentPosition + step) % List.length list + 1

        nextValue =
            currentValue + 1

        nextList =
            List.take nextPos list
                ++ [ nextValue ]
                ++ List.drop nextPos list
    in
        if currentValue == inserts then
            list
        else
            spin step inserts nextPos nextValue nextList



-- Part 2


spinFind : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
spinFind findPos findVal step inserts currentPosition currentValue currentLength =
    let
        nextPos =
            (currentPosition + step) % currentLength + 1

        nextValue =
            currentValue + 1

        nextLength =
            currentLength + 1

        nextFindVal =
            if nextPos == findPos then
                nextValue
            else
                findVal
    in
        if currentValue == inserts then
            findVal
        else
            spinFind findPos nextFindVal step inserts nextPos nextValue nextLength



-- Main


input =
    328


part1 =
    spin input 2017 0 0 [ 0 ]


part2 =
    spinFind 1 0 input 50000000 0 0 1


main =
    Answer.render part1 part2
