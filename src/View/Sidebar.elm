module View.Sidebar exposing (view)

import Code exposing (Instruction)
import Css exposing (..)
import Dict
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Style exposing (..)


view model =
    div
        [ css
            [ order (num 2)
            , Css.property "grid-row" "span 2"
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
                    |> List.foldr (++) []
                    |> dl []

            Err err ->
                text <| Debug.toString <| err
        , h2 [] [ text "Stack" ]
        , model.code
            |> Code.getStack
            |> List.map (\instruction -> li [ css [ monospaced ] ] [ text <| Debug.toString <| instruction ])
            |> ul []
        ]


viewSub : ( String, List Instruction ) -> List (Html Msg)
viewSub ( name, instructions ) =
    dt [ css [ monospaced ] ] [ strong [] [ text ("sub " ++ name ++ "()") ] ]
        :: List.map (\instruction -> dd [ css [ monospaced ] ] [ text <| Debug.toString instruction ]) instructions
