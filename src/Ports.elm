port module Ports exposing
    ( InfoForElm(..)
    , InfoForOutside(..)
    , getInfoFromOutside
    , sendInfoOutside
    )

import Command exposing (Command(..))
import Json.Decode as JD
import Json.Encode as JE


type InfoForOutside
    = Eval String


sendInfoOutside : InfoForOutside -> Cmd msg
sendInfoOutside info =
    (case info of
        Eval code ->
            { tag = "Eval", data = JE.string code }
    )
        |> Debug.log "InfoForOutside"
        |> infoForOutside


type InfoForElm
    = CommandCalled Command


getInfoFromOutside : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
getInfoFromOutside tagger onError =
    infoForElm
        (\outsideInfo ->
            outsideInfo
                |> Debug.log "InfoForElm"
                |> decodeInfoFromOutside tagger onError
        )


decodeInfoFromOutside : (InfoForElm -> msg) -> (String -> msg) -> GenericOutsideData -> msg
decodeInfoFromOutside tagger onError outsideInfo =
    case outsideInfo.tag of
        "CommandCalled" ->
            case
                outsideInfo.data
                    |> JD.decodeValue Command.decoder
            of
                Ok command ->
                    tagger <| CommandCalled command

                Err err ->
                    onError (JD.errorToString err)

        _ ->
            onError <| "Unexpected info from outside: " ++ Debug.toString outsideInfo


type alias GenericOutsideData =
    { tag : String
    , data : JE.Value
    }


port infoForOutside : GenericOutsideData -> Cmd msg


port infoForElm : (GenericOutsideData -> msg) -> Sub msg
