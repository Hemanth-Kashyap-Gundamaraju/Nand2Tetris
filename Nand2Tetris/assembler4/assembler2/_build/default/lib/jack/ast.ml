type segment =
  |Argument |Local  |Static |Constant  |This  |That  |Pointer  |Temp
(* check that int is positive *)
type f = string (*functions*)
type l = string (*labels*)
type n = int (*number of arguments in function*)
type  instrs  =
  (* stack operations *)
  |Push of  segment*int 
  |Pull of  segment*int
  (* arithmatic instructions *)
  | Add |Sub | Neg  | Eql | Gt  | List  | Band  | Bor  | Bnot
  (* control flow instruction *)
  | Goto of l
  | Ifgoto of l
  (* functions *)
  |Call of f*n
  |Return 
type blockvm = {label:l; body :  instrs list   }
type funkshan = {name : f;  numofvar:n; } 