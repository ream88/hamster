module Code exposing (Code, Function(..), Instruction(..), Model, append, init, parse, parser, pop, prepend, setInstructions)

import Parser exposing (..)


type Sub
    = Sub String (List Instruction)


type Instruction
    = SubCall String
    | If Function (List Instruction)
    | While Function (List Instruction)



-- NOTE: Should Code be exported?


type alias Code =
    { instructions : List Instruction, subs : List Sub }


type alias Model =
    Result (List DeadEnd) Code


init : Model
init =
    Ok { instructions = [], subs = [] }


pop : Model -> ( Maybe Instruction, List Instruction )
pop model =
    model
        |> Result.map
            (\{ instructions } ->
                case instructions of
                    head :: tail ->
                        ( Just head, tail )

                    [] ->
                        ( Nothing, [] )
            )
        |> Result.withDefault ( Nothing, [] )


prepend : Instruction -> Model -> Model
prepend instruction =
    Result.map (\code -> { code | instructions = instruction :: code.instructions })


append : Instruction -> Model -> Model
append instruction =
    Result.map (\code -> { code | instructions = code.instructions ++ [ instruction ] })


setInstructions : List Instruction -> Model -> Model
setInstructions newInstructions =
    Result.map (\code -> { code | instructions = newInstructions })


parse : String -> Model -> Model
parse text model =
    Parser.run parser text


parser : Parser Code
parser =
    instructionsParser
        |> Parser.map (\instructions -> { instructions = instructions, subs = [] })


instructionsParser : Parser (List Instruction)
instructionsParser =
    loop [] instructionsParserHelper


next : List Instruction -> Instruction -> Step (List Instruction) a
next instructions instruction =
    Loop (instruction :: instructions)


done : List Instruction -> Step a (List Instruction)
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
            |= subCallParser
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed (done instructions)
        ]


subCallParser : Parser Instruction
subCallParser =
    succeed ()
        |. chompWhile Char.isAlpha
        |. symbol "("
        |. symbol ")"
        |> getChompedString
        |> andThen
            (\name ->
                case String.filter Char.isAlpha name of
                    "go" ->
                        succeed (SubCall "go")

                    "turnLeft" ->
                        succeed (SubCall "turnLeft")

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
