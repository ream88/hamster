module Instructions exposing (Function(..), Instruction(..), parser)

import Parser exposing (..)


type Instruction
    = Go
    | RotateLeft
    | If Function (List Instruction)
    | While Function (List Instruction)


type Function
    = Not Function
    | Free


parser : Parser (List Instruction)
parser =
    instructionsParser


instructionsParser : Parser (List Instruction)
instructionsParser =
    loop [] instructionsParserHelper


next : List Instruction -> Instruction -> Step (List Instruction) a
next instructions instruction =
    Loop (instruction :: instructions)


done instructions =
    Done (List.reverse instructions)


instructionsParserHelper : List Instruction -> Parser (Step (List Instruction) (List Instruction))
instructionsParserHelper instructions =
    oneOf
        [ succeed (next instructions)
            |. spaces
            |= ifParser
            |. spaces
        , succeed (next instructions)
            |. spaces
            |= whileParser
            |. spaces
        , succeed (next instructions)
            |. spaces
            |= goParser
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed (next instructions)
            |. spaces
            |= rotateLeftParser
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed (done instructions)
        ]


goParser : Parser Instruction
goParser =
    succeed Go
        |. keyword "go"
        |. symbol "("
        |. symbol ")"


rotateLeftParser : Parser Instruction
rotateLeftParser =
    succeed RotateLeft
        |. keyword "rotateLeft"
        |. symbol "("
        |. symbol ")"


ifParser : Parser Instruction
ifParser =
    succeed If
        |. keyword "if"
        |. spaces
        |. symbol "("
        |. spaces
        |= functionParser
        |. spaces
        |. symbol ")"
        |. spaces
        |. symbol "{"
        |. spaces
        |= instructionsParser
        |. spaces
        |. symbol "}"


whileParser : Parser Instruction
whileParser =
    succeed While
        |. keyword "while"
        |. spaces
        |. symbol "("
        |. spaces
        |= functionParser
        |. spaces
        |. symbol ")"
        |. spaces
        |. symbol "{"
        |. spaces
        |= instructionsParser
        |. spaces
        |. symbol "}"


functionParser : Parser Function
functionParser =
    oneOf
        [ freeParser
        , lazy (\_ -> notParser)
        ]


freeParser : Parser Function
freeParser =
    succeed Free
        |. keyword "free"
        |. symbol "("
        |. symbol ")"


notParser : Parser Function
notParser =
    succeed Not
        |. keyword "not"
        |. symbol "("
        |= lazy (\_ -> functionParser)
        |. symbol ")"
