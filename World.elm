module World
    exposing
        ( World
        , Tile(..)
        , Direction(..)
        , Error(..)
        , init
        , buildWalls
        , get
        , set
        , findHamster
        , isHamster
        , moveHamster
        , rotateHamster
        )

import Array.Hamt as Array exposing (Array)
import Maybe.Extra as Maybe
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


type Error
    = NoHamster
    | Collision
    | OutOfWorld


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


set : Int -> Int -> Tile -> World -> World
set x y tile world =
    let
        row =
            world
                |> Array.get x
                |> Maybe.withDefault Array.empty
                |> Array.set y tile
    in
        world
            |> Array.set x row


findHamster : World -> Maybe ( Int, Int, Tile )
findHamster world =
    case
        world
            |> Array.toList
            |> List.map (Array.toList >> index isHamster)
            |> List.indexedMap (\x -> Maybe.map (\y -> ( x, y )))
            |> List.filter Maybe.isJust
            |> List.head
            |> Maybe.withDefault Nothing
    of
        Just ( x, y ) ->
            world
                |> get x y
                |> Maybe.map (\tile -> ( x, y, tile ))

        Nothing ->
            Nothing


moveHamster : World -> Result Error World
moveHamster world =
    case findHamster world of
        Just ( x, y, Hamster direction ) ->
            let
                ( newX, newY ) =
                    case direction of
                        North ->
                            ( x, y - 1 )

                        East ->
                            ( x + 1, y )

                        South ->
                            ( x, y + 1 )

                        West ->
                            ( x - 1, y )
            in
                case get newX newY world of
                    Just Wall ->
                        Err Collision

                    Nothing ->
                        Err OutOfWorld

                    _ ->
                        world
                            |> set x y Empty
                            |> set newX newY (Hamster direction)
                            |> Ok

        _ ->
            Err NoHamster


rotateHamster : World -> Result Error World
rotateHamster world =
    case findHamster world of
        Just ( x, y, Hamster direction ) ->
            let
                newDirection =
                    case direction of
                        North ->
                            West

                        East ->
                            North

                        South ->
                            East

                        West ->
                            South
            in
                world
                    |> set x y (Hamster newDirection)
                    |> Ok

        _ ->
            Err NoHamster


get : Int -> Int -> World -> Maybe Tile
get x y world =
    world
        |> Array.get x
        |> Maybe.withDefault Array.empty
        |> Array.get y


index : (a -> Bool) -> List a -> Maybe Int
index =
    indexHelp 0


indexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
indexHelp index fn list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if fn head then
                Just index
            else
                indexHelp (index + 1) fn tail


isHamster : Tile -> Bool
isHamster tile =
    case tile of
        Hamster _ ->
            True

        _ ->
            False
