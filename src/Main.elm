module Main exposing (..)

import Array.Hamt as Array exposing (Array)
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
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
      , interval = 900
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
                ( command, newQueue, newRunning ) =
                    case model.queue of
                        command :: tail ->
                            ( command, tail, model.running )

                        _ ->
                            ( Idle, [], False )
            in
                case executeCommand command model.world of
                    Ok newWorld ->
                        ( { model
                            | world = newWorld
                            , queue = newQueue
                            , error = Nothing
                            , running = newRunning
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
        Time.every ((1000 - model.interval) * Time.millisecond) (always Tick)
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
    div
        [ css
            [ Css.height (vh 100)
            , Css.property "display" "grid"
            , Css.property "grid-template-columns" "auto 400px"
            , Css.property "grid-template-rows" "1fr 1fr"
            , fontFamilies [ "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", .value sansSerif ]
            ]
        ]
        [ div
            [ css
                [ displayFlex
                , justifyContent center
                , alignItems center
                , order (num 1)
                ]
            ]
            [ viewWorld model.world ]
        , div [ css [ order (num 3) ] ] []
        , div
            [ css
                [ order (num 2)
                , Css.property "grid-row" "span 2"
                ]
            ]
            [ viewControls model, text <| toString <| model.error ]
        ]


viewWorld : World -> Html Msg
viewWorld world =
    world
        |> Array.indexedMap
            (\y row ->
                row
                    |> Array.indexedMap (\x -> lazyViewTile x y)
                    |> Array.toList
            )
        |> Array.toList
        |> List.foldr (++) []
        |> div
            [ css
                [ Css.property "display" "grid"
                , Css.property "grid-template-columns" ("repeat(" ++ (toString <| World.width world) ++ ", 1fr)")
                , Css.property "grid-template-rows" ("repeat(" ++ (toString <| World.height world) ++ ", 1fr)")
                , maxWidth (px <| toFloat <| World.width world * 32)
                ]
            ]


lazyViewTile : Int -> Int -> Tile -> Html Msg
lazyViewTile x y tile =
    lazy3 (\x y -> toUnstyled << viewTile x y) x y tile


viewTile : Int -> Int -> Tile -> Html Msg
viewTile x y tile =
    let
        border =
            boxShadow4 (px 0) (px 0) (px 1) (rgba 0 0 0 0.25)

        backgroundImage_ =
            case tile of
                Empty ->
                    batch [ border ]

                Wall ->
                    batch [ backgroundImage (url "assets/brick.png") ]

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
                        batch
                            [ backgroundImage (url "assets/hamster.png")
                            , direction
                                |> directionToDeg
                                |> deg
                                |> rotate
                                |> transform
                            , border
                            ]
    in
        span
            [ css
                [ Css.width (px 32)
                , Css.height (px 32)
                , backgroundSize (pct 100)
                , backgroundImage_
                ]
            , title ("x: " ++ toString x ++ " y: " ++ toString y)
            ]
            []


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
            , Attributes.min "0"
            , Attributes.max "900"
            , step "300"
            , value (toString interval)
            ]
            []
        , queue
            |> List.map (\command -> li [] [ text <| toString command ])
            |> ul []
        ]
