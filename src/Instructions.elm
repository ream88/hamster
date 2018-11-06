module Instructions exposing (Function(..), Instruction(..), parser)

import Parser exposing (..)


type Instruction
    = Go
    | RotateLeft
    | Block (List Instruction)
    | If Function Instruction
    | While Function Instruction


type Function
    = Not Function
    | NotBlocked


parser : Parser (List Instruction)
parser =
    loop [] instructionsParser


instructionsParser : List Instruction -> Parser (Step (List Instruction) (List Instruction))
instructionsParser instructions =
    oneOf
        [ succeed (\instruction -> Loop (instruction :: instructions))
            |. spaces
            |= goParser
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed
            (\instruction -> Loop (instruction :: instructions))
            |. spaces
            |= rotateLeftParser
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse instructions))
        ]


goParser : Parser Instruction
goParser =
    succeed (always Go)
        |= keyword "go"
        |. symbol "("
        |. symbol ")"


rotateLeftParser : Parser Instruction
rotateLeftParser =
    succeed (always RotateLeft)
        |= keyword "rotateLeft"
        |. symbol "("
        |. symbol ")"
