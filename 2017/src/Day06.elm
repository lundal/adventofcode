module Day06 exposing (main)

import Answer
import Dict exposing (Dict)
import List.Extra


findHighest banks =
    banks
        |> List.indexedMap (,)
        |> List.Extra.maximumBy (\elem -> Tuple.second elem)
        |> Maybe.withDefault ( 0, 0 )


distribute : ( Int, Int ) -> List Int -> List Int
distribute ( highIndex, highValue ) banks =
    let
        numBanks =
            List.length banks

        offset idx =
            (idx - highIndex + numBanks - 1) % numBanks

        newValue idx val =
            (if idx /= highIndex then
                val
             else
                0
            )
                + (highValue + numBanks - 1 - offset idx)
                // numBanks
    in
        banks
            |> List.indexedMap newValue


reallocate : List Int -> List Int
reallocate banks =
    distribute (findHighest banks) banks



-- Part 1


reallocationsBeforeLoop1 : Dict (List Int) Bool -> List Int -> Int
reallocationsBeforeLoop1 seenBanks banks =
    if Dict.member banks seenBanks then
        Dict.size seenBanks
    else
        reallocationsBeforeLoop1 (Dict.insert banks True seenBanks) (distribute (findHighest banks) banks)



-- Part 2


reallocationsBeforeLoop2 : Dict (List Int) Int -> List Int -> Int
reallocationsBeforeLoop2 seenBanks banks =
    if Dict.member (Debug.log "banks" banks) seenBanks then
        let
            a =
                Dict.get banks seenBanks |> Maybe.withDefault 0
        in
            (Dict.size seenBanks) - a
    else
        reallocationsBeforeLoop2 (Dict.insert banks (Dict.size seenBanks) seenBanks) (distribute (findHighest banks) banks)



-- Main


input =
    "10 3 15 10 5 15 5 15 9 2 5 8 5 2 3 6"


parse input =
    input
        |> String.words
        |> List.map String.toInt
        |> List.map (Result.withDefault 0)


part1 =
    input
        |> parse
        |> reallocationsBeforeLoop1 Dict.empty


part2 =
    input
        |> parse
        |> reallocationsBeforeLoop2 Dict.empty


main =
    Answer.render part1 part2
