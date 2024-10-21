(* open Bytes *)
open Ast
type minst = bytes
type program = minst list
let inttoinst (value: int) : Bytes.t=
        let bytes = Bytes.create 2 in      
        Bytes.set_uint16_be bytes 0 value; 
    bytes  
    (* let jump_instruction(a : address):int=inttoinst(a lor 0x7007 )
let dest_instruction(a : address):int=inttoinst(a lor  0x7038 )
let comp_instruction(a: address):int=inttoinst( a lor  0xff30) *)
    let pp (p: program)=Bytes.concat Bytes.empty p
    let newinst = Bytes.create 2 

 (* resolves jump to binary*)
    let res_jump_ident (x: jump_ident) =
            match x with    
         | NJP   -> "000"   
         | JGT    -> "001"
         | JEQ    -> "010"
         | JGE    -> "011"
         | JLT    -> "100"
         | JNE    -> "101"
         | JLE    -> "110"
         | JMP    -> "111"

  (* resolves dest to binary*)

    let res_dest_ident (x: dest_ident) =
            match x with 
            | NDT  -> "000"
            | M     -> "001"
            | D     -> "010"
            | MD    -> "011"
            | A     -> "100"
            | AM    -> "101"
            | AD    -> "110"
            | AMD   -> "111"

     (* resolves comp to binary*)

    let res_comp_instruct (x: comp_instruct) =
            match x with 
         | Z      -> "0101010"
         | O      -> "0111111"
         | N      -> "0111010"
         | Dc     -> "0001100"
         | Ac     -> "0110000"
         | Mc     -> "1110000"
         | Dn     -> "0001101"
         | An     -> "0110001"
         | Mn     -> "1110001"
         | Dnn    -> "0001111"
         | Ann    -> "0110011"
         | Mnn    -> "1110011"
         | Di     -> "0011111"
         | Ai     -> "0110111"
         | Mi     -> "1110111"
         | Dd     -> "0001110"
         | Ad     -> "0110010"
         | Md     -> "1110010"
         | DpA    -> "0000010"
         | DpM    -> "1000010"
         | DmA    -> "0010011"
         | DmM    -> "1010011"
         | AmD    -> "0000111"
         | MmD    -> "1000111"
         | DaA    -> "0000000"
         | DaM    -> "1000000"
         | DoA    -> "0010101"
         | DoM    -> "1010101"
    let int_to_inst_str n =
            let rec to_bin n =
                    if n = 0 then ""
    else to_bin (n / 2) ^ string_of_int (n mod 2)
        in
  let bin = to_bin n in
  let padd = String.make (15 - String.length bin) '0' in
  padd ^ bin
    let at(a:int address)= 
            match a with
            | Address  a -> "0" ^ int_to_inst_str a
            | Atlabel  _ -> raise (Failure "should be resolved before reaching this file")
    let encode = function
            | At i -> at i  (* For 'At' instructions, use the `at` function *)
            | C (jump, dest, comp) ->
                            let c = res_comp_instruct comp in
                            let d = res_dest_ident dest in
                            let j = res_jump_ident jump in
                            "111" ^ (c) ^ (d) ^ (j)
            | Loop _ -> raise(Failure "should be resolved by now")      
    let inst_to_machinecode i =
            List.map encode i 
    let assemble (pgm) =
            let binary_instructions = inst_to_machinecode pgm in
            String.concat "\n" binary_instructions

