module View.Sidebar exposing (view)

import Code exposing (Instruction(..))
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model exposing (Model)
import Msg exposing (Msg(..))


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
        [ button [ onClick <| AppendInstruction <| Sub "go" ] [ text "Go" ]
        , button [ onClick <| AppendInstruction <| Sub "turnLeft" ] [ text "Turn Left" ]
        , button [ onClick <| AppendInstruction <| If Code.Free [ Sub "go" ] ] [ text "Go if Free" ]
        , button [ onClick <| AppendInstruction <| While Code.Free [ Sub "go" ] ] [ text "Go while Free" ]
        , button [ onClick <| AppendInstruction <| While Code.Free [ While Code.Free [ Sub "go" ], Sub "turnLeft" ] ] [ text "Run forever in circle" ]
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
        , case model.instructions.instructions of
            Ok instructions ->
                instructions
                    |> List.map (\instruction -> li [] [ text <| Debug.toString <| instruction ])
                    |> ul []

            Err reason ->
                text <| Debug.toString <| reason
        ]
