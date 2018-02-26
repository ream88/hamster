module Main exposing (..)

import Array.Hamt as Array exposing (Array)
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy exposing (..)
import Positive
import World exposing (World, Tile(..), Direction(..), Error(..))


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
    , error : Maybe Error
    }


init : Model
init =
    { world =
        World.init (Positive.fromInt 32) (Positive.fromInt 16)
            |> World.buildWalls
            |> World.set 1 1 (Hamster South)
    , queue = []
    , error = Nothing
    }


type Msg
    = Enqueue Command
    | Tick


type Command
    = Go
    | RotateLeft
    | Idle


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
            in
                case executeCommand command model.world of
                    Ok newWorld ->
                        { model | world = newWorld, queue = newQueue, error = Nothing }

                    Err err ->
                        { model | error = Just err }


executeCommand : Command -> World -> Result Error World
executeCommand command world =
    case command of
        Go ->
            World.moveHamster world

        RotateLeft ->
            World.rotateHamster world

        Idle ->
            Ok world


view : Model -> Html Msg
view { world, queue, error } =
    div [ css [ displayFlex ] ]
        [ viewWorld world
        , text <| toString <| error
        , viewControls queue
        ]


viewWorld : World -> Html Msg
viewWorld model =
    model
        |> Array.indexedMap
            (\x row ->
                row
                    |> Array.indexedMap (lazyViewTile x)
                    |> Array.toList
                    |> Keyed.node "div" []
            )
        |> Array.toList
        |> div
            [ css [ displayFlex ] ]


lazyViewTile : Int -> Int -> Tile -> ( String, Html Msg )
lazyViewTile x y tile =
    ( (toString x) ++ "-" ++ (toString y)
    , lazy3 (\x y -> toUnstyled << viewTile x y) x y tile
    )


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
