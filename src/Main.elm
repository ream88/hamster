module Main exposing (checkExecutionsLimit, executeFunction, executeInstruction, increaseExecutions, main, subscriptions, update)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Events as Events
import Code exposing (Function, Instruction(..))
import Model exposing (Model)
import Msg exposing (Msg(..))
import Positive
import Task
import Task.Extra as Task
import Time exposing (Posix)
import View
import View.Sidebar as Sidebar
import World exposing (Direction(..), Error(..), Tile(..), World)


main : Program () Model Msg
main =
    Browser.document
        { init = Model.init
        , update = update
        , view = View.view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PrependInstruction instruction ->
            ( { model | instructions = Code.prepend instruction model.instructions }, Cmd.none )

        AppendInstruction instruction ->
            ( { model | instructions = Code.append instruction model.instructions }, Cmd.none )

        Toggle ->
            ( { model | running = not model.running }, Cmd.none )

        SetInterval (Just interval) ->
            ( { model | interval = interval }, Cmd.none )

        SetInterval Nothing ->
            ( model, Cmd.none )

        ParseCode string ->
            ( { model | instructions = Code.parse string model.instructions }, Cmd.none )

        Reset ->
            ( { model
                | running = False
                , instructions = Code.init
                , world = Model.initWorld
              }
            , Cmd.none
            )

        Next ->
            ( model, Task.perform Tick Time.now )

        Tick currentTime ->
            if Time.posixToMillis currentTime > Time.posixToMillis model.lastTick + (1000 - model.interval) then
                model.world
                    |> Result.map
                        (\_ ->
                            case model.instructions.instructions of
                                Ok (instruction :: newInstructions) ->
                                    let
                                        ( newWorld, cmd ) =
                                            executeInstruction instruction model.world

                                        newRunning =
                                            newWorld
                                                |> Result.map (always model.running)
                                                |> Result.withDefault False
                                    in
                                    ( { model
                                        | world = newWorld
                                        , running = newRunning
                                        , instructions = Code.setInstructions newInstructions model.instructions
                                        , lastTick = currentTime
                                      }
                                    , cmd
                                    )

                                _ ->
                                    ( { model | running = False }, Cmd.none )
                        )
                    |> Result.withDefault ( model, Cmd.none )

            else
                ( model, Cmd.none )

        SetTile x y tile ->
            ( { model | world = World.set x y tile model.world }, Cmd.none )


executeInstruction : Instruction -> Result Error World -> ( Result Error World, Cmd Msg )
executeInstruction instruction maybeWorld =
    case
        maybeWorld
            |> increaseExecutions
            |> checkExecutionsLimit
    of
        Ok world ->
            case instruction of
                Sub name ->
                    case name of
                        "go" ->
                            ( World.moveHamster (Ok world), Cmd.none )

                        "rotateLeft" ->
                            ( World.rotateHamster (Ok world), Cmd.none )

                        unknown ->
                            ( Err (UnknownInstructionCalled unknown), Cmd.none )

                If function nestedInstructions ->
                    if executeFunction function (Ok world) then
                        ( Ok world
                        , nestedInstructions
                            |> List.map (Task.send << PrependInstruction)
                            |> Cmd.batch
                        )

                    else
                        ( Ok world, Cmd.none )

                While function nestedInstructions ->
                    if executeFunction function (Ok world) then
                        ( Ok world
                        , Cmd.batch
                            [ nestedInstructions
                                |> List.map (Task.send << PrependInstruction)
                                |> Cmd.batch
                            , Task.send (PrependInstruction (While function nestedInstructions))
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
            (\_ ->
                case function of
                    Code.True ->
                        True

                    Code.False ->
                        False

                    Code.Not nestedFunction ->
                        not (executeFunction nestedFunction maybeWorld)

                    Code.Free ->
                        World.isBlocked maybeWorld
            )
        |> Result.withDefault False


checkExecutionsLimit : Result Error World -> Result Error World
checkExecutionsLimit maybeWorld =
    let
        executionsLimit =
            1000
    in
    maybeWorld
        |> Result.andThen
            (\{ executions } ->
                if executions >= executionsLimit then
                    Err ExecutionLimitReached

                else
                    maybeWorld
            )


increaseExecutions : Result Error World -> Result Error World
increaseExecutions =
    Result.map (\world -> { world | executions = world.executions + 1 })


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.running then
            Events.onAnimationFrame Tick

          else
            Sub.none
        ]
