module Style exposing
    ( defaultBorder
    , flatButton
    , monospaced
    , slider
    )

import Css exposing (..)
import Css.Extra as Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)


defaultBorder : Style
defaultBorder =
    batch
        [ backgroundClip paddingBox
        , border2 (px 4) solid
        , Css.property "border-image" "url(border.png) 4 4 stretch"
        ]


monospaced : Style
monospaced =
    fontFamilies [ "VT323" ]


slider : Style
slider =
    batch
        [ defaultBorder
        , margin2 (unit 1) zero
        ]


flatButton : List (Attribute msg) -> List (Html msg) -> Html msg
flatButton =
    styled
        button
        [ defaultBorder
        , outline none
        , monospaced
        , fontSize mediumFontSize
        , padding2 (unit 0.25) (unit 0.75)
        , backgroundColor (hex "fff")
        , boxShadow4 inset (px -4) (px -4) (hex "b6b8c4")
        , active
            [ boxShadow4 inset (px 4) (px 4) (hex "9a9dad")
            , backgroundColor (hex "b6b8c4")
            ]
        ]
