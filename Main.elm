module Main exposing (..)

import Array.Hamt as Array exposing (Array)
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy exposing (..)
import Positive
import Time exposing (Time)
import World exposing (World, Tile(..), Direction(..), Error(..))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { world : World
    , queue : List Command
    , error : Maybe Error
    , running : Bool
    , interval : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { world =
            World.init (Positive.fromInt 32) (Positive.fromInt 16)
                |> World.buildWalls
                |> World.set 1 1 (Hamster South)
      , queue = []
      , error = Nothing
      , running = False
      , interval = 1000
      }
    , Cmd.none
    )


type Msg
    = Enqueue Command
    | Toggle
    | SetInterval (Result String Float)
    | Tick


type Command
    = Go
    | RotateLeft
    | Idle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Enqueue command ->
            ( { model | queue = model.queue ++ [ command ] }, Cmd.none )

        Toggle ->
            ( { model | running = not model.running }, Cmd.none )

        SetInterval (Ok interval) ->
            ( { model | interval = interval }, Cmd.none )

        SetInterval (Err _) ->
            ( model, Cmd.none )

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
                        ( { model
                            | world = newWorld
                            , queue = newQueue
                            , error = Nothing
                            , running = newQueue |> List.isEmpty |> not
                          }
                        , Cmd.none
                        )

                    Err err ->
                        ( { model
                            | error = Just err
                            , running = False
                          }
                        , Cmd.none
                        )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every (model.interval * Time.millisecond) (always Tick)
    else
        Sub.none


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
view model =
    div [ css [ displayFlex ] ]
        [ viewWorld model.world
        , text <| toString <| model.error
        , viewControls model
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


viewControls : Model -> Html Msg
viewControls { queue, running, interval } =
    div []
        [ button [ onClick <| Enqueue Go ] [ text "Go" ]
        , button [ onClick <| Enqueue RotateLeft ] [ text "Rotate Left" ]
        , button [ onClick Tick ] [ text "Next" ]
        , button [ onClick Toggle ]
            [ if running then
                text "Stop"
              else
                text "Start"
            ]
        , input
            [ type_ "range"
            , onInput (SetInterval << String.toFloat)
            , Attributes.min "100"
            , Attributes.max "1000"
            , step "300"
            , value (toString interval)
            ]
            []
        , queue
            |> List.map (\command -> li [] [ text <| toString command ])
            |> ul []
        ]
