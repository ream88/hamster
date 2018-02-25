module WorldTest exposing (..)

import Array.Hamt as Array exposing (Array)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import World exposing (Tile(..), Direction(South), isHamster)
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
                            |> Array.get (width - 1)
                            |> Maybe.withDefault Array.empty
                            |> Array.toList
                            |> List.all isWall
                            |> Expect.true "Has right wall"

                    hasBottomWall world =
                        world
                            |> Array.map (\a -> a |> Array.get (height - 1) |> Maybe.withDefault Empty)
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
                    tiles =
                        World.init (fromInt width) (fromInt height)
                            |> World.setHamster 2 5 (Hamster South)
                            |> Array.toList
                            |> List.map (Array.toList)
                            |> List.foldl (++) []
                in
                    if width > 2 && height > 5 then
                        tiles
                            |> List.any isHamster
                            |> Expect.true "Hamster is in the world"
                    else
                        tiles
                            |> List.any isHamster
                            |> Expect.false "Hamster is not in the world"
        , fuzz2 positive positive "Hamster can be found" <|
            \width height ->
                let
                    world =
                        World.init (fromInt width) (fromInt height)
                            |> World.setHamster 2 5 (Hamster South)
                in
                    if width > 2 && height > 5 then
                        world
                            |> World.getHamster
                            |> Expect.equal (Just ( 2, 5, Hamster South ))
                    else
                        world
                            |> World.getHamster
                            |> Expect.equal Nothing
        ]


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
