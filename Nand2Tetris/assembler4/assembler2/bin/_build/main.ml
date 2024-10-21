
open Hack.Parser

(* Read a file into a string *)
let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* Convert an instruction to a string for printing *)
(* Convert dest_ident to string *)
let string_of_dest_ident d =
  match d with
  | M -> "M"
  | D -> "D"
  | MD -> "MD"
  | A -> "A"
  | AM -> "AM"
  | AD -> "AD"
  | AMD -> "AMD"
  | NDT -> "NDT"
(* Convert jump_ident to string *)
let string_of_jump_ident j =
  match j with
  | JGT -> "JGT"
  | JEQ -> "JEQ"
  | JGE -> "JGE"
  | JLT -> "JLT"
  | JNE -> "JNE"
  | JLE -> "JLE"
  | JMP -> "JMP"
  | NJP -> "NJP"
(* Convert comp_instruct to string *)
let string_of_comp_instruct c =
  match c with
  | Z -> "0"
  | O -> "1"
  | N -> "-1"
  | Dc -> "D"
  | Ac -> "A"
  | Mc -> "M"
  | Dn -> "!D"
  | An -> "!A"
  | Mn -> "!M"
  | Dnn -> "-D"
  | Ann -> "-A"
  | Mnn -> "-M"
  | Di -> "D+1"
  | Ai -> "A+1"
  | Mi -> "M+1"
  | Dd -> "D-1"
  | Ad -> "A-1"
  | Md -> "M-1"
  | DpA -> "D+A"
  | DpM -> "D+M"
  | DmA -> "D-A"
  | DmM -> "D-M"
  | AmD -> "A-D"
  | MmD -> "M-D"
  | DaA -> "D&A"
  | DaM -> "D&M"
  | DoA -> "D|A"
  | DoM -> "D|M"

let string_of_instruction instr =
  match instr with
  | At value -> Printf.sprintf "A-instruction: @%d" value
  | C (jump, dest, comp) -> 
      Printf.sprintf "C-instruction: dest=%s, comp=%s, jump=%s"
        (match dest with   d -> string_of_dest_ident d )
        (string_of_comp_instruct comp)
        (match jump with   j -> string_of_jump_ident j )

(* Main function to parse and handle the program *)
let () =
  (* Get the filename from command-line arguments *)
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let program = read_file filename in
    (* Parse the program using the parser *)
    let parsed_program = parse_program program in
    (* Convert the parsed instructions to strings *)
    let stringified_program = List.map string_of_instruction parsed_program in
    (* Print each instruction as a string *)
    List.iter print_endline stringified_program
read_file hack.asm