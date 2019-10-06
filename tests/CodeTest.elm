module CodeTest exposing (tests)

import Code exposing (Function, Instruction(..))
import Dict exposing (Dict)
import Expect
import Parser exposing (DeadEnd, Problem(..))
import Test exposing (..)


tests : Test
tests =
    describe "Code"
        [ describe "parser"
            [ goInstructionTests
            , turnLeftInstructionTests
            , unknownSubInstructionTests
            , ifInstructionTests
            , whileInstructionTests
            ]
        ]


goInstructionTests : Test
goInstructionTests =
    describe "parses a go instruction"
        [ test "go" <|
            \_ ->
                "go"
                    |> parse
                    |> Expect.equal (Ok [ Call "go" ])
        ]


turnLeftInstructionTests : Test
turnLeftInstructionTests =
    describe "parses a turnLeft instruction"
        [ test "turnLeft" <|
            \_ ->
                "turnLeft"
                    |> parse
                    |> Expect.equal (Ok [ Call "turnLeft" ])
        ]


unknownSubInstructionTests : Test
unknownSubInstructionTests =
    describe "parses any unknown instruction"
        [ test "unknown" <|
            \_ ->
                "unknown"
                    |> parse
                    |> Expect.equal (Ok [ Call "unknown" ])
        ]


ifInstructionTests : Test
ifInstructionTests =
    describe "parses a If instruction"
        [ test "if true end" <|
            \_ ->
                "if true end"
                    |> parse
                    |> Expect.equal (Ok [ If Code.True [] ])
        , test "multiline if (true) {}" <|
            \_ ->
                """if true
                end"""
                    |> parse
                    |> Expect.equal (Ok [ If Code.True [] ])
        , test "if not true end" <|
            \_ ->
                "if not true end"
                    |> parse
                    |> Expect.equal (Ok [ If (Code.Not Code.True) [] ])
        , test "if not free end" <|
            \_ ->
                "if not free end"
                    |> parse
                    |> Expect.equal (Ok [ If (Code.Not Code.Free) [] ])
        ]


whileInstructionTests : Test
whileInstructionTests =
    describe "parses a While instruction"
        [ test "while true end" <|
            \_ ->
                "while true end"
                    |> parse
                    |> Expect.equal (Ok [ While Code.True [] ])
        , test "multiline while true end" <|
            \_ ->
                """while true
                end"""
                    |> parse
                    |> Expect.equal (Ok [ While Code.True [] ])
        , test "while not true end" <|
            \_ ->
                "while not true end"
                    |> parse
                    |> Expect.equal (Ok [ While (Code.Not Code.True) [] ])
        , test "while not free end" <|
            \_ ->
                "while not free end"
                    |> parse
                    |> Expect.equal (Ok [ While (Code.Not Code.Free) [] ])
        ]


firstProblem : List DeadEnd -> Problem
firstProblem =
    List.head
        >> Maybe.map .problem
        >> Maybe.withDefault (Problem "I have no problems!")


parse : String -> Result Problem (List Instruction)
parse code =
    let
        wrappedCode =
            "program main " ++ code ++ " end"
    in
    case Parser.run Code.parser wrappedCode of
        Ok subs ->
            subs
                |> Dict.get "main"
                |> Maybe.map Ok
                |> Maybe.withDefault (Err (Problem "I have no sub main"))

        Err err ->
            Err (firstProblem err)
