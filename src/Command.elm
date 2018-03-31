module Command exposing (Command(..), decoder)

import Json.Decode as JD


type Command
    = Go
    | RotateLeft
    | Idle


decoder : JD.Decoder Command
decoder =
    JD.string
        |> JD.andThen
            (\command ->
                case command of
                    "Go" ->
                        JD.succeed Go

                    "RotateLeft" ->
                        JD.succeed RotateLeft

                    _ ->
                        JD.fail ("Unknown command: " ++ command)
            )
