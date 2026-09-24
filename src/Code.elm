module Code exposing
    ( Code
    , Function(..)
    , Instruction(..)
    , append
    , appendCall
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
   program main do
     if free do
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


{-| Appends a call to the end of the main program's source, creating the main
program if it does not exist yet. Code which cannot be parsed is left untouched.
-}
appendCall : String -> Code -> Code
appendCall name ((Code { source, program }) as code) =
    case ( program, Parser.run endOffsetsParser source ) of
        ( Ok _, Ok endOffsets ) ->
            case Dict.get "main" endOffsets of
                Just offset ->
                    parse
                        (String.trimRight (String.left offset source)
                            ++ "\n  "
                            ++ name
                            ++ "\n"
                            ++ String.dropLeft offset source
                        )

                Nothing ->
                    let
                        main =
                            "program main do\n  " ++ name ++ "\nend"
                    in
                    if String.isEmpty (String.trim source) then
                        parse main

                    else
                        parse (String.trimRight source ++ "\n\n" ++ main)

        _ ->
            code


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
        |. keyword "do"
        |. spaces
        |= lazy (\_ -> instructionsParser)
        |. spaces
        |. keyword "end"


{-| Parses the offset of each program's closing `end` keyword.
-}
endOffsetsParser : Parser (Dict String Int)
endOffsetsParser =
    loop Dict.empty
        (\offsets ->
            oneOf
                [ succeed (\( name, offset ) -> Loop (Dict.insert name offset offsets))
                    |. spaces
                    |= subEndOffsetParser
                    |. spaces
                , succeed ()
                    |> Parser.map (\_ -> Done offsets)
                ]
        )


subEndOffsetParser : Parser ( String, Int )
subEndOffsetParser =
    succeed Tuple.pair
        |. keyword "program"
        |. spaces
        |= nameParser
        |. spaces
        |. keyword "do"
        |. spaces
        |. lazy (\_ -> instructionsParser)
        |. spaces
        |= getOffset
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
        |. keyword "do"
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
        |. keyword "do"
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
    [ "program", "if", "while", "do", "end" ]


nameParser : Parser String
nameParser =
    succeed ()
        |. chompIf Char.isLower
        |. chompWhile (\c -> Char.isAlphaNum c || c == '_')
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
