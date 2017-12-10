module Hex exposing (..)


fromIntMax256 : Int -> String
fromIntMax256 num =
    fromIntMax16 (num // 16) ++ fromIntMax16 (num % 16)


fromIntMax16 : Int -> String
fromIntMax16 num =
    case num of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "a"

        11 ->
            "b"

        12 ->
            "c"

        13 ->
            "d"

        14 ->
            "e"

        15 ->
            "f"

        _ ->
            "x"
