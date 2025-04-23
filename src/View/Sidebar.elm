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
            , justifyContent spaceBetween
            ]
        ]
        [ debugView model
        , div
            [ css
                [ displayFlex
                , flexDirection column
                , justifyContent flexEnd
                ]
            ]
            [ controlButtons model
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
        ]


debugView : Model -> Html Msg
debugView model =
    case Code.getSubs model.code of
        Ok subs ->
            subs
                |> Dict.toList
                |> List.map viewSub
                |> ul
                    [ css
                        [ listStyle none
                        , margin zero
                        , paddingLeft zero
                        ]
                    ]

        -- TODO: Improve error messages
        Err problems ->
            text "Code could not be compiled"


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
        [ flatButton [ css [ marginTop (unit 1) ], onClick <| ParseCode "program main do \n  go\nend" ] [ text "Go" ]
        , flatButton [ css [ marginTop (unit 1) ], onClick <| ParseCode "program main do \n  turn_left\nend" ] [ text "Turn Left" ]
        , flatButton [ css [ marginTop (unit 1) ], onClick <| ParseCode "program main do \n  if free do\n    go\n  end\nend" ] [ text "Go if Free" ]
        , flatButton [ css [ marginTop (unit 1) ], onClick <| ParseCode "program main do \n  while free do\n    go\n  end\nend" ] [ text "Go while Free" ]
        , flatButton [ css [ marginTop (unit 1) ], onClick <| ParseCode "program main do \n  while free do\n    while free do\n      go\n    end\n    turn_left\n  end\nend" ] [ text "Run forever in circle" ]
        ]


viewInstructions : List Instruction -> Html Msg
viewInstructions instructions =
    instructions
        |> List.map (\instruction -> li [] (viewInstruction instruction))
        |> ul
            [ css
                [ listStyle none
                , monospaced
                ]
            ]


viewInstruction : Instruction -> List (Html Msg)
viewInstruction instruction =
    case instruction of
        Call name ->
            [ strong [] [ text name ] ]

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
            strong [] [ text " free " ]


viewSub : ( String, List Instruction ) -> Html Msg
viewSub ( name, instructions ) =
    li
        []
        [ text "program "
        , strong [] [ text name ]
        , viewInstructions instructions
        ]
