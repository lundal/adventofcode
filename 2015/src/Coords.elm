module Coords exposing (..)


type alias Coord =
    ( Int, Int )


origin : Coord
origin =
    ( 0, 0 )


add : Coord -> Coord -> Coord
add a b =
    let
        ( ax, ay ) =
            a

        ( bx, by ) =
            b
    in
        ( ax + bx, ay + by )
