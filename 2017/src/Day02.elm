module Day02 exposing (main)

import Answer
import ListUtils
import Matrix


-- Part 1


checksumLine : List Int -> Int
checksumLine line =
    let
        max =
            line |> List.maximum |> Maybe.withDefault 0

        min =
            line |> List.minimum |> Maybe.withDefault 0
    in
        max - min


checksumMatrix : List (List Int) -> Int
checksumMatrix matrix =
    matrix
        |> List.map checksumLine
        |> List.sum



-- Part 2


evenlyDivisibleValue : Int -> Int -> Int
evenlyDivisibleValue a b =
    if a % b == 0 then
        Debug.log "Dividable" (a // b)
    else
        0


evenlyDivisibleLine : List Int -> Int
evenlyDivisibleLine line =
    line
        |> ListUtils.forAllPermutations2 evenlyDivisibleValue
        |> List.sum


evenlyDivisibleMatrix : List (List Int) -> Int
evenlyDivisibleMatrix matrix =
    matrix
        |> List.map evenlyDivisibleLine
        |> List.sum



-- Main


input : String
input =
    "798 1976 1866 1862 559 1797 1129 747 85 1108 104 2000 248 131 87 95\n201 419 336 65 208 57 74 433 68 360 390 412 355 209 330 135\n967 84 492 1425 1502 1324 1268 1113 1259 81 310 1360 773 69 68 290\n169 264 107 298 38 149 56 126 276 45 305 403 89 179 394 172\n3069 387 2914 2748 1294 1143 3099 152 2867 3082 113 145 2827 2545 134 469\n3885 1098 2638 5806 4655 4787 186 4024 2286 5585 5590 215 5336 2738 218 266\n661 789 393 159 172 355 820 891 196 831 345 784 65 971 396 234\n4095 191 4333 161 3184 193 4830 4153 2070 3759 1207 3222 185 176 2914 4152\n131 298 279 304 118 135 300 74 269 96 366 341 139 159 17 149\n1155 5131 373 136 103 5168 3424 5126 122 5046 4315 126 236 4668 4595 4959\n664 635 588 673 354 656 70 86 211 139 95 40 84 413 618 31\n2163 127 957 2500 2370 2344 2224 1432 125 1984 2392 379 2292 98 456 154\n271 4026 2960 6444 2896 228 819 676 6612 6987 265 2231 2565 6603 207 6236\n91 683 1736 1998 1960 1727 84 1992 1072 1588 1768 74 58 1956 1627 893\n3591 1843 3448 1775 3564 2632 1002 3065 77 3579 78 99 1668 98 2963 3553\n2155 225 2856 3061 105 204 1269 171 2505 2852 977 1377 181 1856 2952 2262"


part1 =
    input
        |> Matrix.parse
        |> checksumMatrix


part2 : Int
part2 =
    input
        |> Matrix.parse
        |> evenlyDivisibleMatrix


main =
    Answer.render part1 part2
