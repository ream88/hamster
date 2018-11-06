module Instructions exposing (Function(..), Instruction(..))


type Instruction
    = Go
    | RotateLeft
    | Block (List Instruction)
    | If Function Instruction
    | While Function Instruction


type Function
    = NotBlocked
