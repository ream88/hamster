module Style exposing (monospaced)

import Css exposing (..)
import Html.Styled.Attributes as Attributes exposing (..)


monospaced =
    fontFamilies [ "Consolas", "Andale Mono WT", "Andale Mono", "Lucida Console", "Lucida Sans Typewriter", "DejaVu Sans Mono", "Bitstream Vera Sans Mono", "Liberation Mono", "Nimbus Mono L", "Monaco", "Courier New", "Courier", .value monospace ]
