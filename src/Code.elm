module Code exposing (Function(..), Instruction(..), Model, append, init, parse, prepend, setInstructions)

import Parser exposing (..)


type alias Model =
    { text : String
    , instructions : Result (List Parser.DeadEnd) (List Instruction)
    }


type Instruction
    = Go
    | RotateLeft
    | If Function (List Instruction)
    | While Function (List Instruction)


init : Model
init =
    { text = "", instructions = Ok [] }


prepend : Instruction -> Model -> Model
prepend instruction model =
    { model | instructions = Result.map (\instructions -> instruction :: instructions) model.instructions }


append : Instruction -> Model -> Model
append instruction model =
    { model | instructions = Result.map (\instructions -> instructions ++ [ instruction ]) model.instructions }


setInstructions : List Instruction -> Model -> Model
setInstructions newInstructions model =
    { model | instructions = Ok newInstructions }


parse : String -> Model -> Model
parse code model =
    { model | text = code, instructions = Parser.run parser code }


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


type Function
    = True
    | False
    | Not Function
    | Free


functionParser : Parser Function
functionParser =
    oneOf
        [ boolParser
        , lazy (\_ -> notParser)
        , freeParser
        ]


boolParser : Parser Function
boolParser =
    oneOf
        [ succeed True |. keyword "true"
        , succeed False |. keyword "false"
        ]


notParser : Parser Function
notParser =
    succeed Not
        |. keyword "not"
        |. symbol "("
        |= lazy (\_ -> functionParser)
        |. symbol ")"


freeParser : Parser Function
freeParser =
    succeed Free
        |. keyword "free"
        |. symbol "("
        |. symbol ")"
