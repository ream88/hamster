module Model exposing (Model, init, initCode, initWorld)

import Code exposing (Code)
import Msg exposing (Msg)
import Positive
import Time exposing (Posix)
import World exposing (Direction(..), Error, Tile(..), World)


type alias Model =
    { world : Result Error World
    , code : Code
    , running : Bool
    , interval : Int
    , lastTick : Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { world = initWorld
      , code = initCode
      , running = False
      , interval = 750
      , lastTick = Time.millisToPosix 0
      }
    , Cmd.none
    )


initWorld : Result Error World
initWorld =
    World.init (Positive.fromInt 32) (Positive.fromInt 16)
        |> World.buildWalls
        |> World.set 1 14 (Hamster East)


initCode : Code
initCode =
    Code.parse "program main do\n  \nend"
