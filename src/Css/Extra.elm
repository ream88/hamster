module Css.Extra exposing
    ( extraLargeFontSize
    , extraLargeLineHeight
    , extraLargeSpace
    , extraLargeWidth
    , largeFontSize
    , largeLineHeight
    , largeSpace
    , largeWidth
    , mediumFontSize
    , mediumLineHeight
    , mediumSpace
    , mediumWidth
    , smallFontSize
    , smallLineHeight
    , smallSpace
    , smallWidth
    , unit
    )

-- https://hihayk.github.io/modulator/

import Css exposing (px)


unit : Float -> Css.Px
unit size =
    px (size * 16)


smallSpace : Css.Px
smallSpace =
    unit 1


mediumSpace : Css.Px
mediumSpace =
    unit 2


largeSpace : Css.Px
largeSpace =
    unit 3


extraLargeSpace : Css.Px
extraLargeSpace =
    unit 8


smallWidth : Css.Px
smallWidth =
    unit 8


mediumWidth : Css.Px
mediumWidth =
    unit 16


largeWidth : Css.Px
largeWidth =
    unit 32


extraLargeWidth : Css.Px
extraLargeWidth =
    unit 64


smallFontSize : Css.Px
smallFontSize =
    px 14


smallLineHeight : Css.Px
smallLineHeight =
    unit 2.5


mediumFontSize : Css.Px
mediumFontSize =
    px 18


mediumLineHeight : Css.Px
mediumLineHeight =
    unit 3


largeFontSize : Css.Px
largeFontSize =
    px 22


largeLineHeight : Css.Px
largeLineHeight =
    unit 4


extraLargeFontSize : Css.Px
extraLargeFontSize =
    px 30


extraLargeLineHeight : Css.Px
extraLargeLineHeight =
    unit 5
