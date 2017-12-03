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


isBetween : Coord -> Coord -> Coord -> Bool
isBetween a b coord =
    let
        ( ax, ay ) =
            a

        ( bx, by ) =
            b

        ( cx, cy ) =
            coord
    in
        cx >= (min ax bx) && cx <= (max ax bx) && cy >= (min ay by) && cy <= (max ay by)
