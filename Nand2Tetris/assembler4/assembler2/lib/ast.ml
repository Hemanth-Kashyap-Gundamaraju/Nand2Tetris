type 'v address = 
| Address of int | Atlabel of string
(* defining a seperate type for jump *)
type jump_ident = 
  | NJP | JGT | JEQ | JGE | JLT | JNE | JLE | JMP
(* defining all destinations *)
type dest_ident = 
  | NDT | M | D | MD | A | AM | AD | AMD
(* defining all compute instructions *)
type comp_instruct =
  | Z | O | N | Dc | Ac | Mc | Dn | An | Mn | Dnn | Ann | Mnn | Di | Ai | Mi | Dd | Ad | Md | DpA | DpM | DmA | DmM | AmD | MmD | DaA | DaM | DoA | DoM

type 'v inst = 
  | At of 'v address
  | C of jump_ident  * dest_ident  * comp_instruct 
  | Loop of string
type 'v block = 'v inst list
type 'v lblock = string * 'v block
type 'v program = 'v lblock list

(* let resolve (pg : string program) = ([] : address block)

let resolvelabel (pg : string program) = ([] : address list) *)

 module StringMap = Map.Make(String)

let rec resolve_labels adrs program symbol_table =
  match program with
  | [] -> symbol_table
  | (label, instructions) :: rest ->
      if StringMap.mem label symbol_table then
        (* failwith ("Label " ^ label ^ " defined multiple times") *)
        resolve_labels adrs rest symbol_table
      else
        let new_symbol_table = StringMap.add label adrs symbol_table in
        let new_adrs = adrs + List.length instructions in
        resolve_labels new_adrs rest new_symbol_table 
