open Assembler.Ast
open Ast
type address = 
  | Function of f
  | Label of l
  | Segment of segment
  | SP 
  | Constant of int
