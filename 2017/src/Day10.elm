module Day10 exposing (main, knotHash)

import Answer
import Bitwise
import Char
import Encoding
import List.Extra


rotateLeft : Int -> List a -> List a
rotateLeft n list =
    List.drop n list ++ List.take n list


rotateRight : Int -> List a -> List a
rotateRight n list =
    rotateLeft (List.length list - n) list


reverseFirst : Int -> List a -> List a
reverseFirst n list =
    List.reverse (List.take n list) ++ List.drop n list


hash : Int -> Int -> List Int -> List Int
hash rounds size lengths =
    hashInternal rounds 0 0 (List.range 0 (size - 1)) [] lengths


hashInternal : Int -> Int -> Int -> List Int -> List Int -> List Int -> List Int
hashInternal rounds position skipSize input processedLengths remainingLengths =
    case remainingLengths of
        head :: tail ->
            let
                nextRemainingLengths =
                    tail

                nextProcessedLengths =
                    processedLengths ++ [ head ]

                nextInput =
                    input
                        |> rotateLeft position
                        |> reverseFirst head
                        |> rotateRight position

                nextSkipSize =
                    skipSize + 1

                nextPosition =
                    (position + head + skipSize) % List.length input
            in
                hashInternal rounds nextPosition nextSkipSize nextInput nextProcessedLengths nextRemainingLengths

        [] ->
            if rounds <= 1 then
                input
            else
                hashInternal (rounds - 1) position skipSize input [] processedLengths



-- Part 2


parseLengths : String -> List Int
parseLengths string =
    (string |> String.toList |> List.map Char.toCode) ++ [ 17, 31, 73, 47, 23 ]


densify : List Int -> List Int
densify sparse =
    sparse
        |> List.Extra.groupsOf 16
        |> List.map (\l -> List.foldl Bitwise.xor 0 l)


knotHash encoder input =
    input
        |> parseLengths
        |> hash 64 256
        |> densify
        |> List.map encoder
        |> String.concat



-- Main


input1 =
    [ 199, 0, 255, 136, 174, 254, 227, 16, 51, 85, 1, 2, 22, 17, 7, 192 ]


input2 =
    "199,0,255,136,174,254,227,16,51,85,1,2,22,17,7,192"


part1 =
    input1
        |> hash 1 256
        |> List.take 2
        |> List.product


part2 =
    knotHash Encoding.intToHex input2


main =
    Answer.render part1 part2
