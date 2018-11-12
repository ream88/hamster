module View exposing (view)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Events as Events
import Code exposing (Function, Instruction(..))
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Lazy exposing (..)
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
            ]
        ]
        [ div
            [ css
                [ displayFlex
                , justifyContent center
                , alignItems center
                , order (num 1)
                ]
            ]
            [ viewWorld model.world ]
        , div [ css [ order (num 3) ] ]
            [ textarea
                [ css
                    [ Css.width (pct 100)
                    , Css.height (pct 100)
                    , fontFamilies [ "Consolas", "Andale Mono WT", "Andale Mono", "Lucida Console", "Lucida Sans Typewriter", "DejaVu Sans Mono", "Bitstream Vera Sans Mono", "Liberation Mono", "Nimbus Mono L", "Monaco", "Courier New", "Courier", .value monospace ]
                    ]
                , onInput ParseCode
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
                            |> Array.indexedMap (\x -> lazyViewTile x y)
                            |> Array.toList
                    )
                |> Array.toList
                |> List.foldr (++) []
                |> div
                    [ css
                        [ Css.property "display" "grid"
                        , Css.property "grid-template-columns" ("repeat(" ++ (String.fromInt <| World.width maybeWorld) ++ ", 1fr)")
                        , Css.property "grid-template-rows" ("repeat(" ++ (String.fromInt <| World.height maybeWorld) ++ ", 1fr)")
                        , maxWidth (px <| toFloat <| World.width maybeWorld * 32)
                        ]
                    ]

        Err err ->
            text <| Debug.toString err


lazyViewTile : Int -> Int -> Tile -> Html Msg
lazyViewTile x y tile =
    lazy3 viewTile x y tile


viewTile : Int -> Int -> Tile -> Html Msg
viewTile x y tile =
    let
        border =
            boxShadow4 (px 0) (px 0) (px 1) (rgba 0 0 0 0.25)

        backgroundImage_ =
            case tile of
                Empty ->
                    batch [ border ]

                Wall ->
                    batch [ backgroundImage (url "assets/brick.png") ]

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
                        [ backgroundImage (url "assets/hamster.png")
                        , direction
                            |> directionToDeg
                            |> deg
                            |> rotate
                            |> transform
                        , border
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
            [ Css.width (px 32)
            , Css.height (px 32)
            , backgroundSize (pct 100)
            , backgroundImage_
            ]
        , title ("( " ++ String.fromInt x ++ ", " ++ String.fromInt y ++ " )")
        , onClick changeTile
        ]
        []
