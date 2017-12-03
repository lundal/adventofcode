module Day03v1 exposing (main)

import Answer


ringEdge n =
    2 * n + 1


ringMax n =
    (ringEdge n) ^ 2


ringMin n =
    if n == 0 then
        0
    else
        ringMax (n - 1) + 1


ring n =
    ( ringMin n, ringMax n )


findRing num =
    findRingInternal num 0


findRingInternal num r =
    let
        ( min, max ) =
            ring r
    in
        if num >= min && num <= max then
            r
        else
            findRingInternal num (r + 1)


findDistance square =
    let
        r =
            findRing square

        min =
            ringMin r

        max =
            ringMax r

        edge =
            ringEdge r

        xpos =
            min + (r - 1)

        ypos =
            xpos + edge - 1

        xneg =
            ypos + edge - 1

        yneg =
            xneg + edge - 1

        offset =
            [ square - xpos, square - ypos, square - xneg, square - yneg ]
                |> List.map abs
                |> List.minimum
                |> Maybe.withDefault 0
    in
        r + offset


input : Int
input =
    361527


part1 =
    findDistance input


part2 =
    0


main =
    Answer.render part1 (findRing 10)
