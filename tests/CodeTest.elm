module CodeTest exposing (tests)

import Code exposing (Function, Instruction(..))
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
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ SubCall "go" ], subs = [] })
        , test "go() ;" <|
            \_ ->
                "go() ;"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ SubCall "go" ], subs = [] })
        , test "go () ;" <|
            \_ ->
                "go () ;"
                    |> Parser.run Code.parser
                    |> Result.mapError firstProblem
                    |> Expect.equal (Err (ExpectingSymbol "("))
        ]


turnLeftInstructionTests : Test
turnLeftInstructionTests =
    describe "parses a turnLeft instruction"
        [ test "turnLeft();" <|
            \_ ->
                "turnLeft();"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ SubCall "turnLeft" ], subs = [] })
        , test "turnLeft() ;" <|
            \_ ->
                "turnLeft() ;"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ SubCall "turnLeft" ], subs = [] })
        , test "turnLeft () ;" <|
            \_ ->
                "turnLeft () ;"
                    |> Parser.run Code.parser
                    |> Result.mapError firstProblem
                    |> Expect.equal (Err (ExpectingSymbol "("))
        ]


unknownSubInstructionTests : Test
unknownSubInstructionTests =
    describe "parses any unknown instruction"
        [ test "unknown();" <|
            \_ ->
                "unknown();"
                    |> Parser.run Code.parser
                    |> Result.mapError firstProblem
                    |> Expect.equal (Err (Problem "unknown is not defined"))
        , test "unknown() ;" <|
            \_ ->
                "unknown() ;"
                    |> Parser.run Code.parser
                    |> Result.mapError firstProblem
                    |> Expect.equal (Err (Problem "unknown is not defined"))
        ]


ifInstructionTests : Test
ifInstructionTests =
    describe "parses a If instruction"
        [ test "if (true) {}" <|
            \_ ->
                "if (true) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ If Code.True [] ], subs = [] })
        , test "multiline if (true) {}" <|
            \_ ->
                """if (true) {
                }"""
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ If Code.True [] ], subs = [] })
        , test "if ( true ) {}" <|
            \_ ->
                "if ( true ) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ If Code.True [] ], subs = [] })
        , test "if (free()) {}" <|
            \_ ->
                "if (free()) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ If Code.Free [] ], subs = [] })
        , test "if ( free() ) {}" <|
            \_ ->
                "if ( free() ) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ If Code.Free [] ], subs = [] })
        ]


whileInstructionTests : Test
whileInstructionTests =
    describe "parses a While instruction"
        [ test "while (true) {}" <|
            \_ ->
                "while (true) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ While Code.True [] ], subs = [] })
        , test "multiline while (true) {}" <|
            \_ ->
                """while (true) {
                }"""
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ While Code.True [] ], subs = [] })
        , test "while ( true ) {}" <|
            \_ ->
                "while ( true ) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ While Code.True [] ], subs = [] })
        , test "while (free()) {}" <|
            \_ ->
                "while (free()) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ While Code.Free [] ], subs = [] })
        , test "while ( free() ) {}" <|
            \_ ->
                "while ( free() ) {}"
                    |> Parser.run Code.parser
                    |> Expect.equal (Ok { instructions = [ While Code.Free [] ], subs = [] })
        ]


firstProblem : List DeadEnd -> Problem
firstProblem =
    List.head
        >> Maybe.map .problem
        >> Maybe.withDefault (Problem "I have no problems!")
