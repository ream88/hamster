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
        [ test "go();" <|
            \_ ->
                "go();"
                    |> parse
                    |> Expect.equal (Ok [ SubCall "go" ])
        , test "go() ;" <|
            \_ ->
                "go() ;"
                    |> parse
                    |> Expect.equal (Ok [ SubCall "go" ])
        , test "go () ;" <|
            \_ ->
                "go () ;"
                    |> parse
                    |> Expect.equal (Err (ExpectingSymbol "("))
        ]


turnLeftInstructionTests : Test
turnLeftInstructionTests =
    describe "parses a turnLeft instruction"
        [ test "turnLeft();" <|
            \_ ->
                "turnLeft();"
                    |> parse
                    |> Expect.equal (Ok [ SubCall "turnLeft" ])
        , test "turnLeft() ;" <|
            \_ ->
                "turnLeft() ;"
                    |> parse
                    |> Expect.equal (Ok [ SubCall "turnLeft" ])
        , test "turnLeft () ;" <|
            \_ ->
                "turnLeft () ;"
                    |> parse
                    |> Expect.equal (Err (ExpectingSymbol "("))
        ]


unknownSubInstructionTests : Test
unknownSubInstructionTests =
    describe "parses any unknown instruction"
        [ test "unknown();" <|
            \_ ->
                "unknown();"
                    |> parse
                    |> Expect.equal (Ok [ SubCall "unknown" ])
        , test "unknown() ;" <|
            \_ ->
                "unknown() ;"
                    |> parse
                    |> Expect.equal (Ok [ SubCall "unknown" ])
        ]


ifInstructionTests : Test
ifInstructionTests =
    describe "parses a If instruction"
        [ test "if (true) {}" <|
            \_ ->
                "if (true) {}"
                    |> parse
                    |> Expect.equal (Ok [ If Code.True [] ])
        , test "multiline if (true) {}" <|
            \_ ->
                """if (true) {
                }"""
                    |> parse
                    |> Expect.equal (Ok [ If Code.True [] ])
        , test "if ( true ) {}" <|
            \_ ->
                "if ( true ) {}"
                    |> parse
                    |> Expect.equal (Ok [ If Code.True [] ])
        , test "if (free()) {}" <|
            \_ ->
                "if (free()) {}"
                    |> parse
                    |> Expect.equal (Ok [ If Code.Free [] ])
        , test "if ( free() ) {}" <|
            \_ ->
                "if ( free() ) {}"
                    |> parse
                    |> Expect.equal (Ok [ If Code.Free [] ])
        ]


whileInstructionTests : Test
whileInstructionTests =
    describe "parses a While instruction"
        [ test "while (true) {}" <|
            \_ ->
                "while (true) {}"
                    |> parse
                    |> Expect.equal (Ok [ While Code.True [] ])
        , test "multiline while (true) {}" <|
            \_ ->
                """while (true) {
                }"""
                    |> parse
                    |> Expect.equal (Ok [ While Code.True [] ])
        , test "while ( true ) {}" <|
            \_ ->
                "while ( true ) {}"
                    |> parse
                    |> Expect.equal (Ok [ While Code.True [] ])
        , test "while (free()) {}" <|
            \_ ->
                "while (free()) {}"
                    |> parse
                    |> Expect.equal (Ok [ While Code.Free [] ])
        , test "while ( free() ) {}" <|
            \_ ->
                "while ( free() ) {}"
                    |> parse
                    |> Expect.equal (Ok [ While Code.Free [] ])
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
            "sub main() {" ++ code ++ "}"
    in
    case Parser.run Code.parser wrappedCode of
        Ok subs ->
            subs
                |> Dict.get "main"
                |> Maybe.map Ok
                |> Maybe.withDefault (Err (Problem "I have no sub main"))

        Err err ->
            Err (firstProblem err)
