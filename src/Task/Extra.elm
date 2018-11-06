module Task.Extra exposing (send)

import Task


send : msg -> Cmd msg
send msg =
    msg
        |> Task.succeed
        |> Task.perform identity
