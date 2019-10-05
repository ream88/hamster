module View.Sidebar exposing (view)

import Code exposing (Function(..), Instruction(..))
import Css exposing (..)
import Css.Extra as Css exposing (..)
import Dict
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Parser
import Style exposing (..)


view model =
    aside
        [ css
            [ order (num 2)
            , Css.property "grid-row" "span 2"
            , backgroundColor (hex "fff")
            , padding (px 16)
            , defaultBorder
            , marginLeft (px 32)
            , displayFlex
            , flexDirection column
            , justifyContent flexEnd
            ]
        ]
        [ debugView model
        , controlButtons model
        , input
            [ type_ "range"
            , css [ slider ]
            , onInput (SetInterval << String.toInt)
            , Attributes.min "0"
            , Attributes.max "1000"
            , step "1"
            , value (String.fromInt model.interval)
            ]
            []
        , cheatButtons model
        ]


debugView : Model -> Html Msg
debugView model =
    case model.code |> Code.getSubs of
        Ok subs ->
            text <| Debug.toString <| subs

        -- TODO: Improve error messages
        Err problems ->
            text <| "Problems parsing the code" ++ Debug.toString problems


controlButtons : Model -> Html Msg
controlButtons model =
    div
        [ css
            [ displayFlex
            , justifyContent spaceBetween
            ]
        ]
        [ flatButton [ onClick Toggle ]
            [ if model.running then
                text "Stop"

              else
                text "Start"
            ]
        , flatButton [ onClick Reset ] [ text "Reset" ]
        ]


cheatButtons : Model -> Html Msg
cheatButtons model =
    div
        [ css
            [ displayFlex
            , justifyContent spaceBetween
            , flexWrap Css.wrap
            ]
        ]
        [ flatButton [ css [ marginTop (unit 1) ], onClick <| ParseCode "sub main() {\n  go();\n}" ] [ text "Go" ]
        , flatButton [ css [ marginTop (unit 1) ], onClick <| ParseCode "sub main() {\n  turnLeft();\n}" ] [ text "Turn Left" ]
        , flatButton [ css [ marginTop (unit 1) ], onClick <| ParseCode "sub main() {\n  if (free()) {\n    go();\n  }\n}" ] [ text "Go if Free" ]
        , flatButton [ css [ marginTop (unit 1) ], onClick <| ParseCode "sub main() {\n  while (free()) {\n    go();\n  }\n}" ] [ text "Go while Free" ]
        , flatButton [ css [ marginTop (unit 1) ], onClick <| ParseCode "sub main() {\n  while (free()) {\n    while (free()) {\n      go();\n    }\n    turnLeft();\n  }\n}" ] [ text "Run forever in circle" ]
        ]


viewInstructions : List Instruction -> Html Msg
viewInstructions instructions =
    instructions
        |> List.map (\instruction -> li [] (viewInstruction instruction))
        |> ul [ css [ monospaced ] ]


viewInstruction : Instruction -> List (Html Msg)
viewInstruction instruction =
    case instruction of
        SubCall name ->
            [ strong [] [ text (name ++ "();") ] ]

        If fun instructions ->
            [ text "if ", viewFunction fun, viewInstructions instructions ]

        While fun instructions ->
            [ text "while ", viewFunction fun, viewInstructions instructions ]


viewFunction : Function -> Html Msg
viewFunction fun =
    case fun of
        Code.True ->
            strong [] [ text " true " ]

        Code.False ->
            strong [] [ text " false " ]

        Code.Not nestedFun ->
            strong [] [ text " not ", viewFunction nestedFun ]

        Code.Free ->
            strong [] [ text " free() " ]


viewSub : ( String, List Instruction ) -> Html Msg
viewSub ( name, instructions ) =
    li
        []
        [ text "sub "
        , strong [] [ text name ]
        , text "()"
        , viewInstructions instructions
        ]
