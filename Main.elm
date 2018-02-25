module Main exposing (..)

import Array.Hamt as Array exposing (Array)
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)


type alias Model =
    Array (Array Tile)


type Tile
    = Empty
    | Wall
    | Hamster


type alias Positive =
    Maybe Int


fromInt : Int -> Positive
fromInt int =
    if int > 0 then
        Just int
    else
        Nothing


main : Program Never Model msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }


init : Model
init =
    initWorld (fromInt 32) (fromInt 16)
        |> buildWalls
        |> setHamster 1 4


initWorld : Positive -> Positive -> Model
initWorld width height =
    case ( width, height ) of
        ( Just width, Just height ) ->
            Empty
                |> always
                |> Array.initialize height
                |> always
                |> Array.initialize width

        _ ->
            Debug.crash "Please provide only positive width and height"


buildWalls : Model -> Model
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


width : Model -> Int
width =
    Array.length


height : Model -> Int
height model =
    model |> Array.get 0 |> Maybe.withDefault Array.empty |> Array.length


setHamster : Int -> Int -> Model -> Model
setHamster x y world =
    let
        row =
            world
                |> Array.get x
                |> Maybe.withDefault Array.empty
                |> Array.set y Hamster
    in
        world
            |> Array.set x row


update : msg -> Model -> Model
update msg model =
    model


view : Model -> Html msg
view =
    viewWorld


viewWorld : Model -> Html msg
viewWorld model =
    model
        |> Array.indexedMap
            (\x row ->
                row
                    |> Array.indexedMap (viewTile x)
                    |> Array.toList
                    |> div []
            )
        |> Array.toList
        |> div
            [ css [ displayFlex ] ]


viewTile : Int -> Int -> Tile -> Html msg
viewTile x y tile =
    let
        backgroundImage =
            case tile of
                Empty ->
                    Nothing

                Wall ->
                    Just "assets/brick.png"

                Hamster ->
                    Just "assets/hamster.png"
    in
        div
            [ css
                ([ Css.width (px 32)
                 , Css.height (px 32)
                 , displayFlex
                 , justifyContent center
                 , alignItems center
                 ]
                    ++ (if x == 0 then
                            []
                        else
                            [ borderLeft3 (px 1) solid (rgba 0 0 0 0.2) ]
                       )
                    ++ (if y == 0 then
                            []
                        else
                            [ borderTop3 (px 1) solid (rgba 0 0 0 0.2) ]
                       )
                )
            ]
            [ case backgroundImage of
                Just src_ ->
                    img
                        [ src src_
                        , css
                            [ Css.width (px 32)
                            , Css.height (px 32)
                            ]
                        ]
                        []

                Nothing ->
                    text ""
            ]
