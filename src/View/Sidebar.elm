module View.Sidebar exposing (view)

import Code exposing (Function(..), Instruction(..))
import Css exposing (..)
import Dict
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Parser
import Style exposing (..)


view model =
    div
        [ css
            [ order (num 2)
            , Css.property "grid-row" "span 2"
            , backgroundColor (hex "fff")
            , padding (px 16)
            , border3 (px 2) solid (hex "000")
            , marginLeft (px 32)
            ]
        ]
        [ viewControls model ]


viewControls : Model -> Html Msg
viewControls model =
    div []
        [ button [ onClick <| ParseCode "sub main() {\n  go();\n}" ] [ text "Go" ]
        , button [ onClick <| ParseCode "sub main() {\n  turnLeft();\n}" ] [ text "Turn Left" ]
        , button [ onClick <| ParseCode "sub main() {\n  if (free()) {\n    go();\n  }\n}" ] [ text "Go if Free" ]
        , button [ onClick <| ParseCode "sub main() {\n  while (free()) {\n    go();\n  }\n}" ] [ text "Go while Free" ]
        , button [ onClick <| ParseCode "sub main() {\n  while (free()) {\n    while (free()) {\n      go();\n    }\n    turnLeft();\n  }\n}" ] [ text "Run forever in circle" ]
        , button [ onClick Next ] [ text "Next" ]
        , button [ onClick Toggle ]
            [ if model.running then
                text "Stop"

              else
                text "Start"
            ]
        , button [ onClick Reset ] [ text "Reset" ]
        , input
            [ type_ "range"
            , onInput (SetInterval << String.toInt)
            , Attributes.min "0"
            , Attributes.max "1000"
            , step "250"
            , value (String.fromInt model.interval)
            ]
            []
        , h2 [] [ text "Parser Result" ]
        , case Code.getSubs model.code of
            Ok subs ->
                subs
                    |> Dict.toList
                    |> List.map viewSub
                    |> ul []

            Err err ->
                text <| Parser.deadEndsToString err
        , h2 [] [ text "Stack" ]
        , viewInstructions <| Code.getStack <| model.code
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
