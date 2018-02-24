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
    initWorld (fromInt 10) (fromInt 10)
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
                (\left ->
                    Array.indexedMap
                        (\top tile ->
                            if top == 0 || top == height_ then
                                Wall
                            else if left == 0 || left == width_ then
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
        |> Array.map
            (\row ->
                row
                    |> Array.map (\tile -> div [] [ text <| toString <| tile ])
                    |> Array.toList
                    |> div []
            )
        |> Array.toList
        |> div [ css [ displayFlex ] ]
