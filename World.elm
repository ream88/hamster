module World
    exposing
        ( World
        , Tile(..)
        , Direction(..)
        , init
        , buildWalls
        , setHamster
        )

import Array.Hamt as Array exposing (Array)
import Positive exposing (Positive)


type alias World =
    Array (Array Tile)


type Tile
    = Empty
    | Wall
    | Hamster Direction


type Direction
    = North
    | East
    | South
    | West


init : Positive -> Positive -> World
init width height =
    case ( width, height ) of
        ( Just width, Just height ) ->
            Empty
                |> always
                |> Array.initialize height
                |> always
                |> Array.initialize width

        _ ->
            Debug.crash "Please provide only positive width and height"


buildWalls : World -> World
buildWalls model =
    let
        height_ =
            height model - 1

        width_ =
            width model - 1
    in
        model
            |> Array.indexedMap
                (\x ->
                    Array.indexedMap
                        (\y tile ->
                            if y == 0 || y == height_ then
                                Wall
                            else if x == 0 || x == width_ then
                                Wall
                            else
                                tile
                        )
                )


width : World -> Int
width =
    Array.length


height : World -> Int
height model =
    model |> Array.get 0 |> Maybe.withDefault Array.empty |> Array.length


setHamster : Int -> Int -> Tile -> World -> World
setHamster x y hamster world =
    let
        row =
            world
                |> Array.get x
                |> Maybe.withDefault Array.empty
                |> Array.set y hamster
    in
        world
            |> Array.set x row
