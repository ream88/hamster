module Main exposing (Model, Msg(..), init, lazyViewTile, main, subscriptions, update, view, viewControls, viewTile, viewWorld)

import Array exposing (Array)
import Browser
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Lazy exposing (..)
import Instructions exposing (Function(..), Instruction(..), parser)
import Parser
import Positive
import Task
import Task.Extra as Task
import Time
import World exposing (Direction(..), Error(..), Tile(..), World)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }


type alias Model =
    { world : Result Error World
    , instructions : Result (List Parser.DeadEnd) (List Instruction)
    , running : Bool
    , interval : Float
    , code : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { world =
            World.init (Positive.fromInt 32) (Positive.fromInt 16)
                |> World.buildWalls
                |> World.set 1 1 (Hamster South)
      , instructions = Ok []
      , running = False
      , interval = 750
      , code = ""
      }
    , Cmd.none
    )


type Msg
    = PrependInstruction Instruction
    | AppendInstruction Instruction
    | Toggle
    | SetInterval (Maybe Float)
    | Tick
    | SetCode String
    | ParseCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PrependInstruction instruction ->
            ( { model | instructions = Result.map (\instructions -> instruction :: instructions) model.instructions }, Cmd.none )

        AppendInstruction instruction ->
            ( { model | instructions = Result.map (\instructions -> instructions ++ [ instruction ]) model.instructions }, Cmd.none )

        Toggle ->
            ( { model | running = not model.running }, Cmd.none )

        SetInterval (Just interval) ->
            ( { model | interval = interval }, Cmd.none )

        SetInterval Nothing ->
            ( model, Cmd.none )

        SetCode code ->
            ( { model | code = code }, Cmd.none )

        ParseCode ->
            ( { model | instructions = Parser.run Instructions.parser model.code }, Cmd.none )

        Tick ->
            model.world
                |> Result.map
                    (\_ ->
                        case model.instructions of
                            Ok (instruction :: newInstructions) ->
                                let
                                    ( newWorld, cmd ) =
                                        executeInstruction instruction model.world

                                    newParseCodening =
                                        case newWorld of
                                            Err _ ->
                                                False

                                            _ ->
                                                model.running
                                in
                                ( { model | world = newWorld, running = newParseCodening, instructions = Ok newInstructions }, cmd )

                            _ ->
                                ( { model | running = False }, Cmd.none )
                    )
                |> Result.withDefault ( model, Cmd.none )


executeInstruction : Instruction -> Result Error World -> ( Result Error World, Cmd Msg )
executeInstruction instruction maybeWorld =
    case
        maybeWorld
            |> increaseExecutions
            |> checkExecutionsLimit
    of
        Ok world ->
            case instruction of
                Go ->
                    ( World.moveHamster (Ok world), Cmd.none )

                RotateLeft ->
                    ( World.rotateHamster (Ok world), Cmd.none )

                Block instructions ->
                    ( Ok world, instructions |> List.map (Task.send << PrependInstruction) |> Cmd.batch )

                If function nestedInstruction ->
                    if executeFunction function (Ok world) then
                        ( Ok world, Task.send (PrependInstruction nestedInstruction) )

                    else
                        ( Ok world, Cmd.none )

                While function nestedInstruction ->
                    if executeFunction function (Ok world) then
                        ( Ok world
                        , Cmd.batch
                            [ Task.send (PrependInstruction nestedInstruction)
                            , Task.send (PrependInstruction (While function nestedInstruction))
                            ]
                        )

                    else
                        ( Ok world, Cmd.none )

        Err reason ->
            ( Err reason, Cmd.none )


executeFunction : Function -> Result Error World -> Bool
executeFunction function maybeWorld =
    maybeWorld
        |> Result.map
            (\world ->
                case function of
                    Not nestedFunction ->
                        not (executeFunction nestedFunction maybeWorld)

                    NotBlocked ->
                        World.isBlocked maybeWorld
            )
        |> Result.withDefault False


executionsLimit : Int
executionsLimit =
    1000


checkExecutionsLimit : Result Error World -> Result Error World
checkExecutionsLimit world =
    case world of
        Ok { executions } ->
            if executions >= executionsLimit then
                Err ExecutionLimitReached

            else
                world

        err ->
            err


increaseExecutions : Result Error World -> Result Error World
increaseExecutions =
    Result.map (\world -> { world | executions = world.executions + 1 })


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
        , div [ css [ order (num 3) ] ]
            [ textarea
                [ css
                    [ Css.width (pct 100)
                    , Css.height (pct 100)
                    ]
                , onInput SetCode
                , value model.code
                ]
                []
            ]
        , div
            [ css
                [ order (num 2)
                , Css.property "grid-row" "span 2"
                ]
            ]
            [ viewControls model ]
        ]


viewWorld : Result Error World -> Html Msg
viewWorld maybeWorld =
    case maybeWorld of
        Ok world ->
            world.tiles
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
                        , Css.property "grid-template-columns" ("repeat(" ++ (String.fromInt <| World.width maybeWorld) ++ ", 1fr)")
                        , Css.property "grid-template-rows" ("repeat(" ++ (String.fromInt <| World.height maybeWorld) ++ ", 1fr)")
                        , maxWidth (px <| toFloat <| World.width maybeWorld * 32)
                        ]
                    ]

        Err err ->
            text <| Debug.toString err


lazyViewTile : Int -> Int -> Tile -> Html Msg
lazyViewTile x y tile =
    lazy3 viewTile x y tile


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
                        directionToDeg direction_ =
                            case direction_ of
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
        , title ("x: " ++ String.fromInt x ++ " y: " ++ String.fromInt y)
        ]
        []


viewControls : Model -> Html Msg
viewControls model =
    div []
        [ button [ onClick <| AppendInstruction Go ] [ text "Go" ]
        , button [ onClick <| AppendInstruction <| Block [ Go, Go, Go, Go, Go ] ] [ text "Go 5x times" ]
        , button [ onClick <| AppendInstruction RotateLeft ] [ text "Rotate Left" ]
        , button [ onClick <| AppendInstruction <| If NotBlocked Go ] [ text "Go if NotBlocked" ]
        , button [ onClick <| AppendInstruction <| While NotBlocked Go ] [ text "Go while NotBlocked" ]
        , button [ onClick <| AppendInstruction <| While NotBlocked (Block [ While NotBlocked Go, RotateLeft, While NotBlocked Go, RotateLeft, While NotBlocked Go, RotateLeft, While NotBlocked Go, RotateLeft ]) ] [ text "ParseCode in circle forever" ]
        , button [ onClick Tick ] [ text "Next" ]
        , button [ onClick Toggle ]
            [ if model.running then
                text "Stop"

              else
                text "Start"
            ]
        , button [ onClick ParseCode ] [ text "Parse Code" ]
        , input
            [ type_ "range"
            , onInput (SetInterval << String.toFloat)
            , Attributes.min "0"
            , Attributes.max "1000"
            , step "250"
            , value (String.fromFloat model.interval)
            ]
            []
        , case model.instructions of
            Ok instructions ->
                instructions
                    |> List.map (\instruction -> li [] [ text <| Debug.toString <| instruction ])
                    |> ul []

            Err reason ->
                text <| Debug.toString <| reason
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.running then
            Time.every (1000 - model.interval) (always Tick)

          else
            Sub.none
        ]
