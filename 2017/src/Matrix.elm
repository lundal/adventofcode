module Matrix exposing (..)


parseLine : String -> List Int
parseLine line =
    line
        |> String.split " "
        |> List.map String.toInt
        |> List.map (Result.withDefault 0)


parse : String -> List (List Int)
parse matrix =
    matrix
        |> String.lines
        |> List.map parseLine
