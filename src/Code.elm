module Code exposing (Function(..), Instruction(..), Model, append, init, parse, parser, prepend, setInstructions)

import Parser exposing (..)


type alias Model =
    { text : String
    , instructions : Result (List Parser.DeadEnd) (List Instruction)
    }


type Instruction
    = Sub String
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
            |= subParser
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed (done instructions)
        ]


subParser : Parser Instruction
subParser =
    succeed ()
        |. chompWhile Char.isAlpha
        |. symbol "("
        |. symbol ")"
        |> getChompedString
        |> andThen
            (\name ->
                case String.filter Char.isAlpha name of
                    "go" ->
                        succeed (Sub "go")

                    "rotateLeft" ->
                        succeed (Sub "rotateLeft")

                    unknown ->
                        problem (unknown ++ " is not defined")
            )


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
