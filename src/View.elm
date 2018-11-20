module View exposing (view)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Events as Events
import Code exposing (Function, Instruction(..))
import Css exposing (..)
import Css.Extra as Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy as Lazy
import Model exposing (Model)
import Msg exposing (Msg(..))
import View.Sidebar as Sidebar
import World exposing (Direction(..), Error(..), Tile(..), World)


view : Model -> Document Msg
view model =
    Document "Hamster" [ toUnstyled (mainView model) ]


mainView : Model -> Html Msg
mainView model =
    div
        [ css
            [ Css.height (vh 100)
            , Css.property "display" "grid"
            , Css.property "grid-template-columns" "auto 400px"
            , Css.property "grid-template-rows" "1fr 1fr"
            , fontFamilies [ "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", .value sansSerif ]
            , padding mediumSpace
            , boxSizing borderBox
            , backgroundImage (url "background.svg")
            , fontSize mediumFontSize
            , lineHeight mediumLineHeight
            ]
        ]
        [ div
            [ css
                [ displayFlex
                , maxHeight (px (16 * 32))
                , justifyContent flexStart
                , alignItems flexStart
                , order (num 1)
                ]
            ]
            [ viewWorld model.world ]
        , div
            [ css
                [ order (num 3)
                , paddingTop mediumSpace
                ]
            ]
            [ textarea
                [ css
                    [ Css.width (pct 100)
                    , Css.height (pct 100)
                    , border3 (px 2) solid (hex "000")
                    , padding smallSpace
                    , boxSizing borderBox
                    , fontFamilies [ "Consolas", "Andale Mono WT", "Andale Mono", "Lucida Console", "Lucida Sans Typewriter", "DejaVu Sans Mono", "Bitstream Vera Sans Mono", "Liberation Mono", "Nimbus Mono L", "Monaco", "Courier New", "Courier", .value monospace ]
                    , resize none
                    , focus
                        [ outline none
                        , borderColor (hex "F08F46")
                        ]
                    ]
                , onInput ParseCode
                , value (Code.getSource model.code)
                ]
                []
            ]
        , Sidebar.view model
        ]


viewWorld : Result Error World -> Html Msg
viewWorld maybeWorld =
    case maybeWorld of
        Ok world ->
            world.tiles
                |> Array.indexedMap
                    (\y row ->
                        row
                            |> Array.indexedMap (\x tile -> ( String.fromInt x ++ "," ++ String.fromInt y, lazyViewTile x y tile ))
                            |> Array.toList
                    )
                |> Array.toList
                |> List.foldr (++) []
                |> Keyed.node "div"
                    [ css
                        [ Css.property "display" "grid"
                        , Css.property "grid-template-columns" ("repeat(" ++ (String.fromInt <| World.width maybeWorld) ++ ", 1fr)")
                        , Css.property "grid-template-rows" ("repeat(" ++ (String.fromInt <| World.height maybeWorld) ++ ", 1fr)")
                        , maxWidth (px <| toFloat <| World.width maybeWorld * 32)
                        ]
                    ]

        Err err ->
            viewError err


lazyViewTile : Int -> Int -> Tile -> Html Msg
lazyViewTile x y tile =
    Lazy.lazy3 viewTile x y tile


viewTile : Int -> Int -> Tile -> Html Msg
viewTile x y tile =
    let
        backgroundImage_ =
            case tile of
                Empty ->
                    batch
                        [ backgroundColor (hex "fff")
                        , backgroundImage (url "empty.svg")
                        ]

                Wall ->
                    batch
                        [ backgroundImage <| linearGradient2 toRight (stop2 (hex "7f4c3d") (pct 0)) (stop2 (hex "7f4c3d") (pct 50)) [ stop2 (hex "804c3e") (pct 75), stop2 (hex "7f4b3d") (pct 100) ]
                        , backgroundImage (url "wall.png")
                        ]

                Hamster direction ->
                    let
                        directionToDeg direction_ =
                            case direction_ of
                                North ->
                                    180

                                East ->
                                    270

                                South ->
                                    0

                                West ->
                                    90
                    in
                    batch
                        [ backgroundImage <| linearGradient2 toRight (stop2 (hex "82676c00") (pct 0)) (stop2 (hex "bda59a") (pct 50)) [ stop2 (hex "b98f70") (pct 75), stop2 (hex "85605a00") (pct 100) ]
                        , backgroundImage (url "hamster.png")
                        , direction
                            |> directionToDeg
                            |> deg
                            |> rotate
                            |> transform
                        ]

        changeTile =
            case tile of
                Empty ->
                    SetTile x y Wall

                Wall ->
                    SetTile x y Empty

                Hamster _ ->
                    SetTile x y tile
    in
    span
        [ css
            [ Css.width mediumSpace
            , Css.height mediumSpace
            , backgroundSize (pct 100)
            , backgroundImage_
            ]
        , title ("( " ++ String.fromInt x ++ ", " ++ String.fromInt y ++ " )")
        , onClick changeTile
        ]
        []


viewError : Error -> Html Msg
viewError error =
    (case error of
        InvalidWorldSize ->
            "The given world size is not valid"

        NoHamster ->
            "There is no hamster in the world"

        Collision ->
            "The hamster collided with a wall"

        OutOfWorld ->
            "The hamster has fallen out of the world"

        ExecutionLimitReached ->
            "Your code run for too long"

        UnknownInstructionCalled instruction ->
            "The sub \"" ++ instruction ++ "\" was never defined"
    )
        |> text
