module CodeTest exposing (tests)

import Code exposing (Function, Instruction(..))
import Expect
import Parser exposing (DeadEnd, Problem(..))
import Test exposing (..)


tests : Test
tests =
    describe "Code"
        [ describe "parse"
            [ goInstructionTests
            , rotateLeftInstructionTests
            , ifInstructionTests
            , whileInstructionTests
            ]
        ]


goInstructionTests : Test
goInstructionTests =
    describe "parses a Go instruction"
        [ test "go();" <|
            \_ ->
                "go();"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ Go ])
        , test "go() ;" <|
            \_ ->
                "go() ;"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ Go ])
        , test "go () ;" <|
            \_ ->
                "go () ;"
                    |> Parser.run Code.parser
                    |> Result.mapError firstProblem
                    |> Expect.equal (Err (ExpectingSymbol "("))
        ]


rotateLeftInstructionTests : Test
rotateLeftInstructionTests =
    describe "parses a RotateLeft instruction"
        [ test "rotateLeft();" <|
            \_ ->
                "rotateLeft();"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ RotateLeft ])
        , test "rotateLeft() ;" <|
            \_ ->
                "rotateLeft() ;"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ RotateLeft ])
        , test "rotateLeft () ;" <|
            \_ ->
                "rotateLeft () ;"
                    |> Parser.run Code.parser
                    |> Result.mapError firstProblem
                    |> Expect.equal (Err (ExpectingSymbol "("))
        ]


ifInstructionTests : Test
ifInstructionTests =
    describe "parses a If instruction"
        [ test "if (true) {}" <|
            \_ ->
                "if (true) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ If Code.True [] ])
        , test "multiline if (true) {}" <|
            \_ ->
                """if (true) {
                }"""
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ If Code.True [] ])
        , test "if ( true ) {}" <|
            \_ ->
                "if ( true ) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ If Code.True [] ])
        , test "if (free()) {}" <|
            \_ ->
                "if (free()) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ If Code.Free [] ])
        , test "if ( free() ) {}" <|
            \_ ->
                "if ( free() ) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ If Code.Free [] ])
        ]


whileInstructionTests : Test
whileInstructionTests =
    describe "parses a While instruction"
        [ test "while (true) {}" <|
            \_ ->
                "while (true) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ While Code.True [] ])
        , test "multiline while (true) {}" <|
            \_ ->
                """while (true) {
                }"""
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ While Code.True [] ])
        , test "while ( true ) {}" <|
            \_ ->
                "while ( true ) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ While Code.True [] ])
        , test "while (free()) {}" <|
            \_ ->
                "while (free()) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ While Code.Free [] ])
        , test "while ( free() ) {}" <|
            \_ ->
                "while ( free() ) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok [ While Code.Free [] ])
        ]


firstProblem : List DeadEnd -> Problem
firstProblem =
    List.head
        >> Maybe.map .problem
        >> Maybe.withDefault (Problem "I have no problems!")
