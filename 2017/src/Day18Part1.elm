module Day18Part1 exposing (main)

import Answer
import Dict exposing (Dict)
import List.Extra


type Argument
    = Reg String
    | Val Int


type Instruction
    = Send Argument
    | Receive String
    | Set String Argument
    | Add String Argument
    | Mul String Argument
    | Mod String Argument
    | Jump Argument Argument
    | Nop


parseArgument : String -> Argument
parseArgument s =
    case String.toInt s of
        Ok i ->
            Val i

        _ ->
            Reg s


parseInstruction : String -> Instruction
parseInstruction line =
    let
        parts =
            String.words line

        inst =
            List.Extra.getAt 0 parts
                |> Maybe.withDefault "x"

        first =
            List.Extra.getAt 1 parts
                |> Maybe.withDefault "x"

        second =
            List.Extra.getAt 2 parts
                |> Maybe.withDefault "x"
    in
        case inst of
            "snd" ->
                Send (first |> parseArgument)

            "rcv" ->
                Receive first

            "set" ->
                Set first (second |> parseArgument)

            "add" ->
                Add first (second |> parseArgument)

            "mul" ->
                Mul first (second |> parseArgument)

            "mod" ->
                Mod first (second |> parseArgument)

            "jgz" ->
                Jump (first |> parseArgument) (second |> parseArgument)

            _ ->
                Nop


type alias Machine =
    { registers : Registers
    , messages : List Int
    , newMessage : Maybe Int
    , messagesSent : Int
    , position : Int
    }


type alias Registers =
    Dict String Int


executeInstruction : Machine -> Instruction -> Machine
executeInstruction machine instruction =
    case instruction of
        Send arg ->
            let
                argValue =
                    getArgValue arg machine.registers
            in
                { machine | newMessage = Just argValue, messagesSent = machine.messagesSent + 1 }

        Receive reg ->
            let
                regValue =
                    getRegValue reg machine.registers
            in
                if regValue /= 0 then
                    -- Ugly, but straight to the point
                    Debug.crash (toString machine.newMessage)
                else
                    machine

        Set reg arg ->
            let
                argValue =
                    getArgValue arg machine.registers
            in
                { machine | registers = Dict.insert reg argValue machine.registers }

        Add reg arg ->
            let
                regValue =
                    getRegValue reg machine.registers

                argValue =
                    getArgValue arg machine.registers

                newValue =
                    regValue + argValue
            in
                { machine | registers = Dict.insert reg newValue machine.registers }

        Mul reg arg ->
            let
                regValue =
                    getRegValue reg machine.registers

                argValue =
                    getArgValue arg machine.registers

                newValue =
                    regValue * argValue
            in
                { machine | registers = Dict.insert reg newValue machine.registers }

        Mod reg arg ->
            let
                regValue =
                    getRegValue reg machine.registers

                argValue =
                    getArgValue arg machine.registers

                newValue =
                    regValue % argValue
            in
                { machine | registers = Dict.insert reg newValue machine.registers }

        Jump arg1 arg2 ->
            let
                arg1Value =
                    getArgValue arg1 machine.registers

                arg2Value =
                    getArgValue arg2 machine.registers
            in
                if arg1Value > 0 then
                    { machine | position = machine.position + arg2Value - 1 }
                else
                    machine

        Nop ->
            machine


getRegValue : String -> Registers -> Int
getRegValue reg registers =
    Dict.get reg registers
        |> Maybe.withDefault 0


getArgValue : Argument -> Registers -> Int
getArgValue arg registers =
    case arg of
        Reg reg ->
            Dict.get reg registers
                |> Maybe.withDefault 0

        Val val ->
            val


stepProgram : Machine -> List Instruction -> Machine
stepProgram machine instructions =
    let
        { registers, messages, newMessage, position } =
            {- Debug.log "machine" -}
            machine

        instruction =
            {- Debug.log "instruction" -}
            (List.Extra.getAt position instructions)
    in
        case instruction of
            Just inst ->
                let
                    newMachine =
                        executeInstruction machine inst
                in
                    { newMachine | position = newMachine.position + 1 }

            Nothing ->
                machine


runProgram : Machine -> List Instruction -> Machine
runProgram machine instructions =
    let
        steppedMachine =
            stepProgram machine instructions
    in
        runProgram steppedMachine instructions



-- Main


input =
    "set i 31\nset a 1\nmul p 17\njgz p p\nmul a 2\nadd i -1\njgz i -2\nadd a -1\nset i 127\nset p 464\nmul p 8505\nmod p a\nmul p 129749\nadd p 12345\nmod p a\nset b p\nmod b 10000\nsnd b\nadd i -1\njgz i -9\njgz a 3\nrcv b\njgz b -1\nset f 0\nset i 126\nrcv a\nrcv b\nset p a\nmul p -1\nadd p b\njgz p 4\nsnd a\nset a b\njgz 1 3\nsnd b\nset f 1\nadd i -1\njgz i -11\nsnd a\njgz f -16\njgz a -19"


part1 =
    let
        instructions =
            input
                |> String.lines
                |> List.map parseInstruction

        machine =
            { registers = Dict.empty, messages = [], newMessage = Nothing, messagesSent = 0, position = 0 }
    in
        runProgram machine instructions


part2 =
    "See Day18Part2.elm"


main =
    Answer.render part1 part2
