module World exposing
    ( Direction(..)
    , Error(..)
    , Tile(..)
    , World
    , buildWalls
    , findHamster
    , get
    , height
    , init
    , isBlocked
    , isHamster
    , moveHamster
    , rotateHamster
    , set
    , width
    )

import Array exposing (Array)
import Maybe.Extra as Maybe
import Positive exposing (Positive)


type alias World =
    { tiles : Array (Array Tile), executions : Int }


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
    | InvalidWorldSize
    | ExecutionLimitReached


init : Positive -> Positive -> Result Error World
init maybeWidth maybeHeight =
    case ( maybeWidth, maybeHeight ) of
        ( Just positiveWidth, Just positiveHeight ) ->
            Ok
                { tiles =
                    Empty
                        |> always
                        |> Array.initialize positiveWidth
                        |> always
                        |> Array.initialize positiveHeight
                , executions = 0
                }

        _ ->
            Err InvalidWorldSize


buildWalls : Result Error World -> Result Error World
buildWalls maybeWorld =
    let
        height_ =
            height maybeWorld - 1

        width_ =
            width maybeWorld - 1
    in
    maybeWorld
        |> Result.map
            (\world ->
                let
                    newTiles =
                        world.tiles
                            |> Array.indexedMap
                                (\y ->
                                    Array.indexedMap
                                        (\x tile ->
                                            if y == 0 || y == height_ then
                                                Wall

                                            else if x == 0 || x == width_ then
                                                Wall

                                            else
                                                tile
                                        )
                                )
                in
                { world | tiles = newTiles }
            )


height : Result Error World -> Int
height maybeWorld =
    maybeWorld
        |> Result.map (Array.length << .tiles)
        |> Result.withDefault 0


width : Result Error World -> Int
width maybeWorld =
    maybeWorld
        |> Result.map (.tiles >> Array.get 0 >> Maybe.withDefault Array.empty >> Array.length)
        |> Result.withDefault 0


set : Int -> Int -> Tile -> Result Error World -> Result Error World
set x y tile =
    Result.map
        (\world ->
            let
                row =
                    world.tiles
                        |> Array.get y
                        |> Maybe.withDefault Array.empty
                        |> Array.set x tile

                newTiles =
                    world.tiles
                        |> Array.set y row
            in
            { world | tiles = newTiles }
        )


get : Int -> Int -> Result Error World -> Maybe Tile
get x y maybeWorld =
    maybeWorld
        |> Result.map (.tiles >> Array.get y >> Maybe.withDefault Array.empty >> Array.get x)
        |> Result.withDefault Nothing


findHamster : Result Error World -> Maybe ( Int, Int, Tile )
findHamster maybeWorld =
    maybeWorld
        |> Result.map
            (\{ tiles } ->
                case
                    tiles
                        |> Array.toList
                        |> List.map (Array.toList >> index isHamster)
                        |> List.indexedMap (\y -> Maybe.map (\x -> ( x, y )))
                        |> List.filter Maybe.isJust
                        |> List.head
                        |> Maybe.withDefault Nothing
                of
                    Just ( x, y ) ->
                        maybeWorld
                            |> get x y
                            |> Maybe.map (\tile -> ( x, y, tile ))

                    Nothing ->
                        Nothing
            )
        |> Result.withDefault Nothing


isBlocked : Result Error World -> Bool
isBlocked maybeWorld =
    maybeWorld
        |> moveHamster
        |> Result.map (always True)
        |> Result.withDefault False


moveHamster : Result Error World -> Result Error World
moveHamster maybeWorld =
    case findHamster maybeWorld of
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
            case get newX newY maybeWorld of
                Just Wall ->
                    Err Collision

                Nothing ->
                    Err OutOfWorld

                _ ->
                    maybeWorld
                        |> set x y Empty
                        |> set newX newY (Hamster direction)

        _ ->
            Err NoHamster


rotateHamster : Result Error World -> Result Error World
rotateHamster maybeWorld =
    case findHamster maybeWorld of
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
            maybeWorld
                |> set x y (Hamster newDirection)

        _ ->
            Err NoHamster


index : (a -> Bool) -> List a -> Maybe Int
index =
    indexHelp 0


indexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
indexHelp index_ fn list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if fn head then
                Just index_

            else
                indexHelp (index_ + 1) fn tail


isHamster : Tile -> Bool
isHamster tile =
    case tile of
        Hamster _ ->
            True

        _ ->
            False
