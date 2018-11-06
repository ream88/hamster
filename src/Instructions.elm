module Instructions exposing (Function(..), Instruction(..))


type Instruction
    = Go
    | RotateLeft
    | Idle
    | Block (List Instruction)
    | If Function Instruction
    | While Function Instruction


type Function
    = NotBlocked
