module StringUtils exposing (..)


rotate : Int -> String -> String
rotate n text =
    String.right n text ++ String.dropRight n text
