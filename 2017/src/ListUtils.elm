module ListUtils exposing (..)

{-| A library for advanced processing of lists.
-}


{-| Call fun on all 2-element permutations of the list, including (a, a).
-}
forAllPermutations2NotUnique : (a -> a -> b) -> List a -> List b
forAllPermutations2NotUnique fun list =
    List.concatMap (\a -> List.map (\b -> fun a b) list) list


{-| Call fun on all 2-element permutations of the list.
-}
forAllPermutations2 : (a -> a -> b) -> List a -> List b
forAllPermutations2 fun list =
    case list of
        head :: tail ->
            List.map (\other -> fun head other) tail
                ++ List.map (\other -> fun other head) tail
                ++ forAllPermutations2 fun tail

        [] ->
            []


{-| Call fun on all 2-element combinations of the list.
-}
forAllCombinations2 : (a -> a -> b) -> List a -> List b
forAllCombinations2 fun list =
    case list of
        head :: tail ->
            List.map (\other -> fun head other) tail
                ++ forAllCombinations2 fun tail

        [] ->
            []
