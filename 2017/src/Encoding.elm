module Encoding exposing (intToBin, intToHex)


intToBin : Int -> String
intToBin num =
    encode toHex 2 num
        |> String.padLeft 8 '0'


intToHex : Int -> String
intToHex num =
    encode toHex 16 num
        |> String.padLeft 2 '0'



-- Encoder


encode : (Int -> String) -> Int -> Int -> String
encode encoder base num =
    encodeInternal "" encoder base num


encodeInternal : String -> (Int -> String) -> Int -> Int -> String
encodeInternal out encoder base num =
    let
        high =
            num // base

        low =
            num % base
    in
        if num > 0 then
            encodeInternal (encoder low ++ out) encoder base high
        else
            out



-- Encodings


toHex : Int -> String
toHex num =
    String.slice num (num + 1) "0123456789abcdef"
