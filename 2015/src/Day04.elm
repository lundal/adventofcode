module Day04 exposing (main)

import Answer
import MD5


lowestNum : String -> String -> Int
lowestNum key prefix =
    lowestNumInternal key prefix 1


lowestNumInternal : String -> String -> Int -> Int
lowestNumInternal key prefix num =
    let
        hash =
            MD5.hex (key ++ toString num)
    in
        if String.startsWith prefix hash then
            num
        else
            lowestNumInternal key prefix (num + 1)



-- Main


input =
    "bgvyzdsv"


part1 =
    lowestNum input "00000"


part2 =
    lowestNum input "000000"


main =
    Answer.render part1 part2
