open Ast

(* Convert a string to jump_ident *)
let jump_ident_of_string s =
  match s with
  | "JGT" -> JGT
  | "JEQ" -> JEQ
  | "JGE" -> JGE
  | "JLT" -> JLT
  | "JNE" -> JNE
  | "JLE" -> JLE
  | "JMP" -> JMP
  | _ -> raise (Failure "Invalid jump identifier")

(* Convert a string to dest_ident *)
let dest_ident_of_string s =
  match s with
  | "M" -> M
  | "D" -> D
  | "MD" -> MD
  | "A" -> A
  | "AM" -> AM
  | "AD" -> AD
  | "AMD" -> AMD
  | _ -> raise (Failure "Invalid destination identifier")

(* Convert a string to comp_instruct *)
let comp_instruct_of_string s =
  match s with
  | "0" -> Z 
  | "1" -> O 
  | "-1" -> N 
  | "D" -> Dc 
  | "A" -> Ac 
  | "M" -> Mc 
  | "!D" -> Dn 
  | "!A" -> An 
  | "!M" -> Mn 
  | "-D" -> Dnn 
  | "-A" -> Ann 
  | "-M" -> Mnn 
  | "D+1" -> Di 
  | "A+1" -> Ai 
  | "M+1" -> Mi 
  | "D-1" -> Dd 
  | "A-1" -> Ad 
  | "M-1" -> Md 
  | "D+A" -> DpA 
  | "D+M" -> DpM 
  | "D-A" -> DmA 
  | "D-M" -> DmM
  | "A-D" -> AmD 
  | "M-D" -> MmD 
  | "D&A" -> DaA 
  | "D&M" -> DaM 
  | "D|A" -> DoA 
  | "D|M" -> DoM
  | _ -> raise (Failure ("Invalid computation instruction: " ^ s))

(* Skip whitespace and comments *)
let  skip_whitespace_and_comments input =
  let input = String.trim input in
  if String.length input = 0 then ""
  else if String.sub input 0 2 = "//" then ""  (* Skip comments *)
  else input

(* Parse L-instruction: (LABEL) *)
let parse_l_instruction input =
  let label = String.sub input 1 (String.length input - 2) in
  Loop label  (* Return the L-instruction *)

(* Parse A-instruction: @value *)
let parse_a_instruction input =
  let input = String.sub input 1 (String.length input - 1) in  (* Skip '@' *)
  if String.length input > 0 && input.[0] >= '0' && input.[0] <= '9' then
    At (Address (int_of_string input))  (* Numeric A-instruction *)
  else
    At (Atlabel input)  (* Symbolic A-instruction *)

(* Parse symbols in A and L-instructions *)
let parse_symbol input =
  String.trim input  (* Return the symbol as-is after trimming whitespace *)

(* Parse C-instruction: dest=comp;jump *)
let parse_c_instruction input =
  let dest_comp_jump = String.split_on_char '=' input in
  let (dest, comp_jump) = match dest_comp_jump with
    | [d; cj] -> (dest_ident_of_string d, cj)
    | [cj] -> (NDT, cj)  (* No destination part *)
    | _ -> raise (Failure "Invalid C-instruction format")
  in
  let comp_jump_parts = String.split_on_char ';' comp_jump in
  let (comp, jump) = match comp_jump_parts with
    | [c; j] -> (comp_instruct_of_string c, jump_ident_of_string j)
    | [c] -> (comp_instruct_of_string c, NJP)  (* No jump part *)
    | _ -> raise (Failure "Invalid computation/jump format")
  in
  C (jump, dest, comp)  (* Return C-instruction *)

(* Parse a line - returning only the instruction *)
let parse_line input =
  let input = String.trim input in
  if String.length input = 0 then
    raise (Failure "Attempted to parse an empty line")
  else if String.get input 0 = '@' then
    parse_a_instruction input  (* Parse A-instruction *)
  else if String.get input 0 = '(' && String.get input (String.length input - 1) = ')' then
    parse_l_instruction input  (* Parse L-instruction *)
  else
    parse_c_instruction input  (* Parse C-instruction *)

(* Parse a complete program without a symbol table *)
let parse_program input  =
  let processed_input = List.map skip_whitespace_and_comments input in
  let cleaned_lines = List.filter (fun line -> String.length line > 0) processed_input in  (* Remove empty lines *)

  (* Convert instructions to lblock format, wrapping each instruction in a list to form a block *)
  let parsed_instructions  = 
    List.map (fun instr -> ("", [parse_line instr])) cleaned_lines  (* Wrap instructions in lblock format *)
  in
  parsed_instructions  (* Return parsed instructions *)
