module StringUtils exposing (..)


splitAt : Int -> String -> ( String, String )
splitAt i s =
    ( String.left i s, String.dropLeft i s )
