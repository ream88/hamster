module Code exposing
    ( Code
    , Function(..)
    , Instruction(..)
    , append
    , getSource
    , getStack
    , getSub
    , getSubs
    , parse
    , parser
    , pop
    , prepend
    , setStack
    )

import Dict exposing (Dict)
import Parser exposing (..)


type Instruction
    = SubCall String
    | If Function (List Instruction)
    | While Function (List Instruction)


type alias Subs =
    Dict String (List Instruction)


type Code
    = Code
        { source : String
        , program : Result (List DeadEnd) Subs
        , stack : List Instruction
        }


parse : String -> Code
parse source =
    Code
        { source = source
        , stack = []
        , program = Parser.run parser source
        }


pop : Code -> ( Maybe Instruction, List Instruction )
pop (Code { stack }) =
    case stack of
        head :: tail ->
            ( Just head, tail )

        [] ->
            ( Nothing, [] )


prepend : Instruction -> Code -> Code
prepend instruction (Code model) =
    Code { model | stack = instruction :: model.stack }


append : Instruction -> Code -> Code
append instruction (Code model) =
    Code { model | stack = model.stack ++ [ instruction ] }


setStack : List Instruction -> Code -> Code
setStack newStack (Code model) =
    Code { model | stack = newStack }


getSub : String -> Code -> Maybe (List Instruction)
getSub name (Code { program }) =
    case program of
        Ok subs ->
            Dict.get name subs

        _ ->
            Nothing


getSubs : Code -> Result (List DeadEnd) Subs
getSubs (Code { program }) =
    program


getSource : Code -> String
getSource (Code { source }) =
    source


getStack : Code -> List Instruction
getStack (Code { stack }) =
    stack


parser : Parser Subs
parser =
    subsParser


subsParser : Parser Subs
subsParser =
    loop Dict.empty subsParserHelper


subsParserHelper : Subs -> Parser (Step Subs Subs)
subsParserHelper subs =
    oneOf
        [ succeed (\( name, instructions ) -> Loop (Dict.insert name instructions subs))
            |. spaces
            |= subParser
            |. spaces
        , succeed () |> Parser.map (\_ -> Done subs)
        ]


instructionsParser : Parser (List Instruction)
instructionsParser =
    loop [] instructionsParserHelper


instructionsParserHelper : List Instruction -> Parser (Step (List Instruction) (List Instruction))
instructionsParserHelper instructions =
    oneOf
        [ succeed (\instruction -> Loop (instruction :: instructions))
            |. spaces
            |= ifParser
            |. spaces
        , succeed (\instruction -> Loop (instruction :: instructions))
            |. spaces
            |= whileParser
            |. spaces
        , succeed (\instruction -> Loop (instruction :: instructions))
            |. spaces
            |= subCallParser
            |. spaces
            |. symbol ";"
            |. spaces
        , succeed () |> Parser.map (\_ -> Done (List.reverse instructions))
        ]


subCallParser : Parser Instruction
subCallParser =
    succeed SubCall
        |= nameParser
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


nameParser : Parser String
nameParser =
    succeed ()
        |. chompWhile Char.isAlpha
        |> getChompedString
        |> andThen
            (\string ->
                if String.length string == 0 then
                    problem "opps"

                else
                    commit string
            )


subParser : Parser ( String, List Instruction )
subParser =
    succeed Tuple.pair
        |. keyword "sub"
        |. spaces
        |= nameParser
        |. symbol "("
        |. symbol ")"
        |. spaces
        |. symbol "{"
        |. spaces
        |= instructionsParser
        |. spaces
        |. symbol "}"
