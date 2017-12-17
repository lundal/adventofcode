module Day14 exposing (main)

import Answer
import Array exposing (Array)
import Day10
import Encoding
import List.Extra


type alias Disk =
    List (List Int)


type alias ArrayDisk =
    Array (Array Int)


parseDisk : Disk -> Int -> String -> Disk
parseDisk rows remaining input =
    let
        row =
            (input ++ "-" ++ toString (remaining - 1))
                |> Day10.knotHash Encoding.intToBin
                |> String.split ""
                |> List.map String.toInt
                |> List.map (Result.withDefault 0)
    in
        if remaining == 0 then
            rows
        else
            parseDisk (row :: rows) (remaining - 1) input



-- Part 1


used : Disk -> Int
used disk =
    disk
        |> List.concatMap identity
        |> List.filter (\e -> e == 1)
        |> List.sum



-- Part 2


tagRegions : Disk -> Disk
tagRegions disk =
    disk
        |> List.indexedMap (\y row -> row |> List.indexedMap (\x cell -> cell * (1 + x + y * 128)))


toArrayDisk : Disk -> ArrayDisk
toArrayDisk disk =
    disk
        |> List.map Array.fromList
        |> Array.fromList


toDisk : ArrayDisk -> Disk
toDisk disk =
    disk
        |> Array.map Array.toList
        |> Array.toList


growRegions : ArrayDisk -> ArrayDisk
growRegions disk =
    let
        nextDisk =
            growRegionsStep 0 0 disk
    in
        if nextDisk == disk then
            disk
        else
            growRegions nextDisk


growRegionsStep : Int -> Int -> ArrayDisk -> ArrayDisk
growRegionsStep y x disk =
    let
        self =
            Just disk
                |> Maybe.andThen (Array.get y)
                |> Maybe.andThen (Array.get x)

        left =
            Just disk
                |> Maybe.andThen (Array.get y)
                |> Maybe.andThen (Array.get (x - 1))

        right =
            Just disk
                |> Maybe.andThen (Array.get y)
                |> Maybe.andThen (Array.get (x + 1))

        up =
            Just disk
                |> Maybe.andThen (Array.get (y - 1))
                |> Maybe.andThen (Array.get x)

        down =
            Just disk
                |> Maybe.andThen (Array.get (y + 1))
                |> Maybe.andThen (Array.get x)

        newSelf =
            if Maybe.withDefault 0 self == 0 then
                0
            else
                [ self, left, right, right, up, down ]
                    |> List.map (Maybe.withDefault 0)
                    |> List.maximum
                    |> Maybe.withDefault 0

        newDisk =
            disk
                |> Array.set y
                    (disk
                        |> Array.get y
                        |> Maybe.withDefault Array.empty
                        |> Array.set x newSelf
                    )

        nextX =
            (x + 1) % 128

        nextY =
            if nextX == 0 then
                Debug.log "y" (y + 1)
            else
                y
    in
        if y > 127 then
            disk
        else
            growRegionsStep nextY nextX newDisk


countRegions : Disk -> Int
countRegions disk =
    disk
        |> List.concat
        |> List.Extra.unique
        |> List.length



-- Main


input =
    "amgozmfv"


part1 =
    input
        |> parseDisk [] 128
        |> used


part2 =
    input
        |> parseDisk [] 128
        |> tagRegions
        |> toArrayDisk
        |> growRegions
        |> toDisk
        |> countRegions
        |> (+) -1


main =
    Answer.render part1 part2
