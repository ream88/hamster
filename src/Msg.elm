module Msg exposing (Msg(..))

import Code exposing (Instruction)
import Time exposing (Posix)
import World exposing (Tile)


type Msg
    = PrependInstruction Instruction
    | AppendInstruction Instruction
    | Toggle
    | SetInterval (Maybe Int)
    | Next
    | Tick Posix
    | ParseCode String
    | Reset
    | SetTile Int Int Tile
