module Main exposing (..)

import Array.Hamt as Array exposing (Array)
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }


type alias Model =
    { world : World
    , queue : List Command
    , error : Maybe String
    }


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


type Command
    = Go
    | RotateLeft
    | Idle


type alias Positive =
    Maybe Int


fromInt : Int -> Positive
fromInt int =
    if int > 0 then
        Just int
    else
        Nothing


init : Model
init =
    { world =
        initWorld (fromInt 32) (fromInt 16)
            |> buildWalls
            |> setHamster 1 1 (Hamster South)
    , queue = []
    , error = Nothing
    }


initWorld : Positive -> Positive -> World
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


executeCommand : Command -> World -> Result String World
executeCommand command world =
    case command of
        Go ->
            Ok world

        RotateLeft ->
            Ok world

        Idle ->
            Ok world


type Msg
    = Enqueue Command
    | Tick


update : Msg -> Model -> Model
update msg model =
    case msg of
        Enqueue command ->
            { model | queue = model.queue ++ [ command ] }

        Tick ->
            let
                ( command, newQueue ) =
                    case model.queue of
                        command :: tail ->
                            ( command, tail )

                        _ ->
                            ( Idle, [] )

                newWorld =
                    executeCommand command model.world
            in
                case newWorld of
                    Ok newWorld ->
                        { model | world = newWorld, queue = newQueue }

                    Err err ->
                        { model | error = Just err }


view : Model -> Html Msg
view { world, queue } =
    div [ css [ displayFlex ] ]
        [ viewWorld world
        , viewControls queue
        ]


viewWorld : World -> Html Msg
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


viewTile : Int -> Int -> Tile -> Html Msg
viewTile x y tile =
    let
        background =
            case tile of
                Empty ->
                    Nothing

                Wall ->
                    Just <| batch [ backgroundImage (url "assets/brick.png") ]

                Hamster direction ->
                    let
                        directionToDeg direction =
                            case direction of
                                North ->
                                    180

                                East ->
                                    270

                                South ->
                                    0

                                West ->
                                    90
                    in
                        Just <|
                            batch
                                [ backgroundImage (url "assets/hamster.png")
                                , direction
                                    |> directionToDeg
                                    |> deg
                                    |> rotate
                                    |> transform
                                ]
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
            [ case background of
                Just backgroundImage ->
                    span
                        [ css
                            [ Css.width (px 32)
                            , Css.height (px 32)
                            , backgroundSize (px 32)
                            , backgroundImage
                            ]
                        ]
                        []

                Nothing ->
                    text ""
            ]


viewControls : List Command -> Html Msg
viewControls queue =
    div []
        [ button [ onClick <| Enqueue Go ] [ text "Go" ]
        , button [ onClick <| Enqueue RotateLeft ] [ text "Rotate Left" ]
        , button [ onClick Tick ] [ text "Next" ]
        , queue |> List.map (\command -> li [] [ text <| toString command ]) |> ul []
        ]
