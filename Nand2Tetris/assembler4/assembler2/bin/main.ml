open Assembler.Parser
open Assembler.Machine
open Assembler.Ast
(*This assembler does not work with symbol labels  
To run this assembler use:
 cd Downloads/assembler4/assembler2 
dune exec assembler 
Enter your Hack code.
To terminate the assembler use ctrl+d *)
module StringMap = Map.Make(String)

(* Predefined Hack symbols *)
let predefined_symbols = [
  ("SP", 0); ("LCL", 1); ("ARG", 2); ("THIS", 3); ("THAT", 4);
  ("R0", 0); ("R1", 1); ("R2", 2); ("R3", 3); ("R4", 4);
  ("R5", 5); ("R6", 6); ("R7", 7); ("R8", 8); ("R9", 9);
  ("R10", 10); ("R11", 11); ("R12", 12); ("R13", 13); ("R14", 14); ("R15", 15);
  ("SCREEN", 16384); ("KBD", 24576)
]

(* Function to read all lines from standard input *)
let read_lines () =
  let rec read_lines_acc acc =
    try
      let line = input_line stdin in
      read_lines_acc (line :: acc)
    with End_of_file -> List.rev acc
  in
  read_lines_acc []

(* First pass: resolve labels *)
let first_pass (program: 'v lblock list) =
  let initial_symbol_table = List.fold_left (fun acc (k, v) -> StringMap.add k v acc) StringMap.empty predefined_symbols in
  resolve_labels 0 program initial_symbol_table

(* Second pass: resolve variables and labels *)
let second_pass program symbol_table =
  let var_address = ref 16 in  (* Start variable addresses at 16 *)

  let rec resolve_instrs block_list current_symbol_table resolved_instrs =
    match block_list with
    | [] -> List.rev resolved_instrs  (* Return the resolved instructions in reverse order *)
    | (_, instructions) :: rest ->  (* Process each block's instructions *)
        let rec resolve_block instrs current_symbol_table resolved_instrs =
          match instrs with
          | [] -> resolved_instrs
          | instr :: rest_instrs ->
              match instr with
              | At (Atlabel label) ->
                  if StringMap.mem label current_symbol_table then
                    (* If the label is already in the symbol table, reuse its address *)
                    let address = StringMap.find label current_symbol_table in
                    resolve_block rest_instrs current_symbol_table (At (Address address) :: resolved_instrs)
                  else
                    (* Allocate a new address for this variable and update the symbol table *)
                    let address = !var_address in
                    var_address := !var_address + 1;  (* Increment for the next variable *)
                    let updated_symbol_table = StringMap.add label address current_symbol_table in
                    resolve_block rest_instrs updated_symbol_table (At (Address address) :: resolved_instrs)
              | Loop _ -> resolve_block rest_instrs current_symbol_table resolved_instrs  (* Skip label instructions *)
              | _ -> resolve_block rest_instrs current_symbol_table (instr :: resolved_instrs)
        in
        let resolved_block = resolve_block instructions current_symbol_table [] in
        resolve_instrs rest current_symbol_table (resolved_block @ resolved_instrs)  (* Append the resolved block *)
  in
  resolve_instrs program symbol_table []  (* Start with an empty resolved instruction list *)

let () =
  (* Read input lines *)
  let program_lines = read_lines () in
  (* Parse the lines into AST *)
  let ast:'v lblock list = parse_program program_lines in

  (* First pass: resolve labels *)
  let symbol_table = first_pass ast in 

  (* Second pass: resolve variables *)
  let resolved_ast = second_pass ast symbol_table in

  (* Convert AST to machine code *)
  let binary_code = assemble resolved_ast in
  (* Print the machine code *)
  print_endline binary_code;

