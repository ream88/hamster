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
        , appendCallTests
        ]


appendCallTests : Test
appendCallTests =
    let
        appendCall name source =
            source
                |> Code.parse
                |> Code.appendCall name
                |> Code.getSource
    in
    describe "appendCall"
        [ test "replaces an empty line in main" <|
            \_ ->
                "program main do\n  \nend"
                    |> appendCall "go"
                    |> Expect.equal "program main do\n  go\nend"
        , test "appends multiple calls" <|
            \_ ->
                "program main do\n  \nend"
                    |> appendCall "go"
                    |> appendCall "turn_left"
                    |> appendCall "go"
                    |> Expect.equal "program main do\n  go\n  turn_left\n  go\nend"
        , test "appends after nested blocks" <|
            \_ ->
                "program main do\n  while free do\n    go\n  end\nend"
                    |> appendCall "turn_left"
                    |> Expect.equal "program main do\n  while free do\n    go\n  end\n  turn_left\nend"
        , test "only touches the main program" <|
            \_ ->
                "program main do\n  go\nend\n\nprogram other do\n  go\nend"
                    |> appendCall "turn_left"
                    |> Expect.equal "program main do\n  go\n  turn_left\nend\n\nprogram other do\n  go\nend"
        , test "creates main if missing" <|
            \_ ->
                "program other do\n  go\nend"
                    |> appendCall "go"
                    |> Expect.equal "program other do\n  go\nend\n\nprogram main do\n  go\nend"
        , test "creates main in empty source" <|
            \_ ->
                ""
                    |> appendCall "go"
                    |> Expect.equal "program main do\n  go\nend"
        , test "updates the parsed program" <|
            \_ ->
                "program main do\n  \nend"
                    |> Code.parse
                    |> Code.appendCall "go"
                    |> Code.appendCall "go"
                    |> Code.getSub "main"
                    |> Expect.equal (Just [ Call "go", Call "go" ])
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
