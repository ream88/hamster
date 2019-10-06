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



{-
   Hamster Script

   ```
   program main
     if free
       go
     end
   end
   ```
-}


type Instruction
    = Call String
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
        , succeed ()
            |> Parser.map (\_ -> Done subs)
        ]


subParser : Parser ( String, List Instruction )
subParser =
    succeed Tuple.pair
        |. keyword "program"
        |. spaces
        |= nameParser
        |. spaces
        |= lazy (\_ -> instructionsParser)
        |. spaces
        |. keyword "end"


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
            -- callParser must be backtrackable, otherwise it parses "end" as a call
            |= backtrackable callParser
            |. spaces
        , succeed () |> Parser.map (\_ -> Done (List.reverse instructions))
        ]


callParser : Parser Instruction
callParser =
    succeed Call
        |= nameParser


ifParser : Parser Instruction
ifParser =
    succeed If
        |. keyword "if"
        |. spaces
        |= functionParser
        |. spaces
        |= instructionsParser
        |. spaces
        |. keyword "end"


whileParser : Parser Instruction
whileParser =
    succeed While
        |. keyword "while"
        |. spaces
        |= functionParser
        |. spaces
        |= instructionsParser
        |. spaces
        |. keyword "end"


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
        |. spaces
        |= lazy (\_ -> functionParser)


freeParser : Parser Function
freeParser =
    succeed Free
        |. keyword "free"


reservedKeywords : List String
reservedKeywords =
    [ "program", "if", "while", "end" ]


nameParser : Parser String
nameParser =
    succeed ()
        |. chompWhile Char.isAlpha
        |> getChompedString
        |> andThen
            (\string ->
                if List.member string reservedKeywords then
                    problem ("keyword \"" ++ string ++ "\" is reserved")

                else if String.length string == 0 then
                    problem "name required"

                else
                    commit string
            )
