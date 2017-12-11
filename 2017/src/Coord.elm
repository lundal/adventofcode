module Coord exposing (..)


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


nextInSpiral : Coord -> Coord
nextInSpiral coord =
    let
        ( x, y ) =
            coord

        r =
            max (abs x) (abs y)
    in
        if y == -r then
            ( x + 1, y )
        else if x == r && y < r then
            ( x, y + 1 )
        else if x == -r && y > -r then
            ( x, y - 1 )
        else
            ( x - 1, y )


adjacent : Coord -> List Coord
adjacent coord =
    let
        ( x, y ) =
            coord
    in
        [ ( x + 1, y + 0 )
        , ( x + 1, y + 1 )
        , ( x + 0, y + 1 )
        , ( x - 1, y + 1 )
        , ( x - 1, y + 0 )
        , ( x - 1, y - 1 )
        , ( x + 0, y - 1 )
        , ( x + 1, y - 1 )
        ]
