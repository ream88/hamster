module MainTest exposing (tests)

import Code exposing (Code, Function, Instruction(..))
import Expect
import Main exposing (..)
import Positive
import Test exposing (..)
import World exposing (Direction(..), Error(..), Tile(..), World)


tests : Test
tests =
    describe "Main"
        [ describe "executeInstruction"
            [ goInstructionTests
            , turnLeftInstructionTests
            , unknownSubInstructionTests

            -- TODO: Both ifInstructionTests and whileInstructionTests needs proper Expect checks
            , ifInstructionTests
            , whileInstructionTests
            , checkExecutionsLimitTests
            ]
        , describe "executeFunction"
            [ trueFunctionTests
            , falseFunctionTests
            , notFunctionTests
            , freeFunctionTests
            ]
        ]


goInstructionTests : Test
goInstructionTests =
    describe "a go instruction"
        [ test "results in a NoHamster Err when no hamster is found" <|
            \_ ->
                sampleWorld
                    |> executeInstruction (Call "go") sampleCode
                    |> Tuple.first
                    |> Expect.equal (Err NoHamster)
        , test "causes the hamster to fall off the world" <|
            \_ ->
                sampleWorld
                    |> World.set 0 0 (Hamster North)
                    |> executeInstruction (Call "go") sampleCode
                    |> Tuple.first
                    |> Expect.equal (Err OutOfWorld)
        , test "causes the hamster to collide with the wall" <|
            \_ ->
                let
                    worldAfterCollision =
                        sampleWorld
                            |> World.set 0 0 (Hamster South)
                            |> World.set 0 1 Wall
                            |> setExecutions 1
                in
                sampleWorld
                    |> World.set 0 0 (Hamster South)
                    |> World.set 0 1 Wall
                    |> executeInstruction (Call "go") sampleCode
                    |> Tuple.first
                    |> Expect.equal (Err (Collision worldAfterCollision))
        , test "moves the hamster one field into the current direction" <|
            \_ ->
                sampleWorld
                    |> World.set 0 0 (Hamster South)
                    |> executeInstruction (Call "go") sampleCode
                    |> Tuple.first
                    |> World.findHamster
                    |> Expect.equal (Just ( 0, 1, Hamster South ))
        ]


turnLeftInstructionTests : Test
turnLeftInstructionTests =
    describe "a turn_left instruction"
        [ test "results in a NoHamster Err when no hamster is found" <|
            \_ ->
                sampleWorld
                    |> executeInstruction (Call "turn_left") sampleCode
                    |> Tuple.first
                    |> Expect.equal (Err NoHamster)
        , test "turns the hamster to the left" <|
            \_ ->
                sampleWorld
                    |> World.set 0 0 (Hamster South)
                    |> executeInstruction (Call "turn_left") sampleCode
                    |> Tuple.first
                    |> World.findHamster
                    |> Expect.equal (Just ( 0, 0, Hamster East ))
        ]


unknownSubInstructionTests : Test
unknownSubInstructionTests =
    describe "any unknown instruction"
        [ test "results in a UnknownInstructionCalled Err when called" <|
            \_ ->
                sampleWorld
                    |> World.set 0 0 (Hamster South)
                    |> executeInstruction (Call "unknown") sampleCode
                    |> Tuple.first
                    |> Expect.equal (Err (UnknownInstructionCalled "unknown"))
        ]


ifInstructionTests : Test
ifInstructionTests =
    describe "a If instruction"
        [ test "prepends the nested instructions onto the stack if equaling to True" <|
            \_ ->
                sampleWorld
                    |> executeInstruction (If Code.True [ Call "go" ]) sampleCode
                    |> Tuple.second
                    |> Expect.notEqual Cmd.none
        , test "does not prepend the nested instructions onto the stack if equaling to False" <|
            \_ ->
                sampleWorld
                    |> executeInstruction (If Code.False [ Call "go" ]) sampleCode
                    |> Tuple.second
                    |> Expect.equal Cmd.none
        ]


whileInstructionTests : Test
whileInstructionTests =
    describe "a While instruction"
        [ test "prepends the nested instructions onto the stack if equaling to True" <|
            \_ ->
                sampleWorld
                    |> executeInstruction (While Code.True [ Call "go" ]) sampleCode
                    |> Tuple.second
                    |> Expect.notEqual Cmd.none
        , test "does not prepend the nested instructions onto the stack if equaling to False" <|
            \_ ->
                sampleWorld
                    |> executeInstruction (While Code.False [ Call "go" ]) sampleCode
                    |> Tuple.second
                    |> Expect.equal Cmd.none
        ]


checkExecutionsLimitTests : Test
checkExecutionsLimitTests =
    describe "checkExecutionsLimit"
        [ test "executes instructions when limit is not reached" <|
            \_ ->
                sampleWorld
                    |> World.set 0 0 (Hamster South)
                    |> setExecutions 0
                    |> executeInstruction (Call "go") sampleCode
                    |> Tuple.first
                    |> World.findHamster
                    |> Expect.equal (Just ( 0, 1, Hamster South ))
        , test "does not execute instructions when limit is reached" <|
            \_ ->
                sampleWorld
                    |> World.set 0 0 (Hamster South)
                    |> setExecutions 999
                    |> executeInstruction (Call "go") sampleCode
                    |> Tuple.first
                    |> Expect.equal (Err ExecutionLimitReached)
        ]


trueFunctionTests : Test
trueFunctionTests =
    describe "a True keyword"
        [ test "equals to True" <|
            \_ ->
                sampleWorld
                    |> executeFunction Code.True
                    |> Expect.equal True
        ]


falseFunctionTests : Test
falseFunctionTests =
    describe "a False keyword"
        [ test "equals to False" <|
            \_ ->
                sampleWorld
                    |> executeFunction Code.False
                    |> Expect.equal False
        ]


notFunctionTests : Test
notFunctionTests =
    describe "a Not function"
        [ test "equals to False if given a True" <|
            \_ ->
                sampleWorld
                    |> executeFunction (Code.Not Code.True)
                    |> Expect.equal False
        , test "equals to True if given a False" <|
            \_ ->
                sampleWorld
                    |> executeFunction (Code.Not Code.False)
                    |> Expect.equal True
        ]


freeFunctionTests : Test
freeFunctionTests =
    describe "a free function"
        [ test "equals to False if there is no hamster in the world" <|
            \_ ->
                sampleWorld
                    |> executeFunction Code.Free
                    |> Expect.equal False
        , test "equals to False if the hamster faces a wall" <|
            \_ ->
                sampleWorld
                    |> World.set 0 0 (Hamster South)
                    |> World.set 0 1 Wall
                    |> executeFunction Code.Free
                    |> Expect.equal False
        , test "equals to False if the hamster faces the edge of the world" <|
            \_ ->
                sampleWorld
                    |> World.set 0 0 (Hamster North)
                    |> executeFunction Code.Free
                    |> Expect.equal False
        , test "equals to True if the hamster can move to the field in front of him" <|
            \_ ->
                sampleWorld
                    |> World.set 0 0 (Hamster South)
                    |> executeFunction Code.Free
                    |> Expect.equal True
        ]


sampleWorld : Result Error World
sampleWorld =
    World.init (Positive.fromInt 10) (Positive.fromInt 10)


sampleCode : Code
sampleCode =
    Code.parse ""



-- TODO: This should probably be internal


setExecutions : Int -> Result Error World -> Result Error World
setExecutions newExecutions =
    Result.map (\world -> { world | executions = newExecutions })
