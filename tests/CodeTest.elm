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
    describe "parses a turn_left instruction"
        [ test "turn_left" <|
            \_ ->
                "turn_left"
                    |> parse
                    |> Expect.equal (Ok [ Call "turn_left" ])
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
        [ test "if true do end" <|
            \_ ->
                "if true do end"
                    |> parse
                    |> Expect.equal (Ok [ If Code.True [] ])
        , test "multiline if true do end" <|
            \_ ->
                """if true do
                end"""
                    |> parse
                    |> Expect.equal (Ok [ If Code.True [] ])
        , test "if not true do end" <|
            \_ ->
                "if not true do end"
                    |> parse
                    |> Expect.equal (Ok [ If (Code.Not Code.True) [] ])
        , test "if not free do end" <|
            \_ ->
                "if not free do end"
                    |> parse
                    |> Expect.equal (Ok [ If (Code.Not Code.Free) [] ])
        ]


whileInstructionTests : Test
whileInstructionTests =
    describe "parses a While instruction"
        [ test "while true do end" <|
            \_ ->
                "while true do end"
                    |> parse
                    |> Expect.equal (Ok [ While Code.True [] ])
        , test "multiline while true do end" <|
            \_ ->
                """while true do
                end"""
                    |> parse
                    |> Expect.equal (Ok [ While Code.True [] ])
        , test "while not true do end" <|
            \_ ->
                "while not true do end"
                    |> parse
                    |> Expect.equal (Ok [ While (Code.Not Code.True) [] ])
        , test "while not free do end" <|
            \_ ->
                "while not free do end"
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
            "program main do " ++ code ++ " end"
    in
    case Parser.run Code.parser wrappedCode of
        Ok subs ->
            subs
                |> Dict.get "main"
                |> Maybe.map Ok
                |> Maybe.withDefault (Err (Problem "I have no sub main"))

        Err err ->
            Err (firstProblem err)
