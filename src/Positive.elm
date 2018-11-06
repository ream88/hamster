module Positive exposing (Positive, fromInt)


type alias Positive =
    Maybe Int


fromInt : Int -> Positive
fromInt int =
    if int > 0 then
        Just int

    else
        Nothing
