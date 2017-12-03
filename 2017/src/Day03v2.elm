module Day03v2 exposing (main)

import Answer
import Coord exposing (Coord)
import Dict exposing (Dict)


type alias Grid =
    Dict Coord Int



-- Part 1


findCoord : Int -> Coord
findCoord value =
    findCoordInternal value 1 Coord.origin


findCoordInternal : Int -> Int -> Coord -> Coord
findCoordInternal value currentValue currentCoord =
    if value == currentValue then
        currentCoord
    else
        findCoordInternal value (currentValue + 1) (Coord.nextInSpiral currentCoord)



-- Part 2


fillGrid fillFunc maxValue =
    fillGridInternal fillFunc maxValue Coord.origin Dict.empty


fillGridInternal : (Coord -> Grid -> Int) -> Int -> Coord -> Grid -> Grid
fillGridInternal fillFunc maxValue coord grid =
    let
        value =
            fillFunc coord grid

        nextGrid =
            Dict.insert coord value grid

        nextCoord =
            Coord.nextInSpiral coord
    in
        if value > maxValue then
            nextGrid
        else
            fillGridInternal fillFunc maxValue nextCoord nextGrid


adjacentValues : Coord -> Grid -> List Int
adjacentValues coord grid =
    coord
        |> Coord.adjacent
        |> List.map (\key -> Dict.get key grid)
        |> List.map (Maybe.withDefault 0)


sumAdjacent : Coord -> Grid -> Int
sumAdjacent coord grid =
    max (adjacentValues coord grid |> List.sum) 1



-- Main


input : Int
input =
    361527


part1 =
    let
        ( x, y ) =
            findCoord input
    in
        x + y


part2 =
    let
        ( key, val ) =
            fillGrid sumAdjacent input
                |> Dict.filter (\k v -> v > input)
                |> Dict.toList
                |> List.head
                |> Maybe.withDefault ( ( 0, 0 ), 0 )
    in
        val


main =
    Answer.render part1 part2
