module Answer exposing (..)

import Html exposing (Html)


render : a -> b -> Html msg
render part1 part2 =
    Html.div []
        [ Html.h4 [] [ Html.text "Part 1" ]
        , Html.p [] [ Html.text (toString part1) ]
        , Html.h4 [] [ Html.text "Part 2" ]
        , Html.p [] [ Html.text (toString part2) ]
        ]
