module WorldTest exposing (..)

import Array.Hamt as Array exposing (Array)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import World exposing (World, Tile(..), Direction(..))
import Positive exposing (fromInt)


worldTests : Test
worldTests =
    describe "init"
        [ fuzz2 positive positive "Always returns width * height tiles" <|
            \width height ->
                World.init (fromInt width) (fromInt height)
                    |> Array.map (Array.length)
                    |> Array.foldl (+) 0
                    |> Expect.equal (width * height)
        , fuzz2 positive positive "Always generates a world with proper height" <|
            \width height ->
                World.init (fromInt width) (fromInt height)
                    |> Array.length
                    |> Expect.equal height
        , fuzz2 positive positive "Always generates a world with proper width" <|
            \width height ->
                World.init (fromInt width) (fromInt height)
                    |> Array.get 0
                    |> Maybe.withDefault Array.empty
                    |> Array.length
                    |> Expect.equal width
        , fuzz2 positive positive "Always surrounds the world with walls" <|
            \width height ->
                let
                    hasTopWall world =
                        world
                            |> Array.get 0
                            |> Maybe.withDefault Array.empty
                            |> Array.toList
                            |> List.all isWall
                            |> Expect.true "Has top wall"

                    hasRightWall world =
                        world
                            |> Array.map (\a -> a |> Array.get (width - 1) |> Maybe.withDefault Empty)
                            |> Array.toList
                            |> List.all isWall
                            |> Expect.true "Has right wall"

                    hasBottomWall world =
                        world
                            |> Array.get (height - 1)
                            |> Maybe.withDefault Array.empty
                            |> Array.toList
                            |> List.all isWall
                            |> Expect.true "Has bottom wall"

                    hasLeftWall world =
                        world
                            |> Array.map (\a -> a |> Array.get 0 |> Maybe.withDefault Empty)
                            |> Array.toList
                            |> List.all isWall
                            |> Expect.true "Has left wall"
                in
                    World.init (fromInt width) (fromInt height)
                        |> World.buildWalls
                        |> Expect.all
                            [ hasTopWall
                            , hasRightWall
                            , hasBottomWall
                            , hasLeftWall
                            ]
        , fuzz2 positive positive "Sets Hamster into the world if possible" <|
            \width height ->
                let
                    world =
                        World.init (fromInt width) (fromInt height)
                            |> World.buildWalls
                            |> World.set 2 5 (Hamster South)
                in
                    if width > 2 && height > 5 then
                        world
                            |> World.findHamster
                            |> Expect.equal (Just ( 2, 5, Hamster South ))
                    else
                        world
                            |> World.findHamster
                            |> Expect.equal Nothing
        , fuzz2 positive positive "Hamster can be rotated" <|
            \width height ->
                let
                    world =
                        World.init (fromInt width) (fromInt height)
                            |> World.buildWalls
                            |> World.set 2 5 (Hamster South)
                in
                    if width > 2 && height > 5 then
                        world
                            |> World.rotateHamster
                            |> Result.withDefault world
                            |> World.findHamster
                            |> Expect.equal (Just ( 2, 5, Hamster East ))
                    else
                        Expect.pass
        , fuzz2 positive positive "Hamster can be moved if the world is big enough" <|
            \width height ->
                let
                    world =
                        World.init (fromInt width) (fromInt height)
                            |> World.buildWalls
                            |> World.set 2 5 (Hamster South)
                            |> World.moveHamster
                in
                    if width > 3 && height > 7 then
                        world
                            |> isOk
                            |> Expect.true "World is Ok"
                    else
                        world
                            |> isOk
                            |> Expect.false "World is not Ok"
        ]


isOk : Result e a -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


isWall : Tile -> Bool
isWall tile =
    case tile of
        Wall ->
            True

        _ ->
            False


positive : Fuzzer Int
positive =
    Fuzz.intRange 1 100
