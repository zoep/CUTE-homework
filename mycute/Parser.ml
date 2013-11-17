open Genlex
open Types

let lexer = make_lexer ["("; ")"; "+"; "-"; "*"; 
                        "then";":"; "else"; ","; "="; 
                        "=="; "<"; ">"; "<="; "_"; "!=";
                        ">="; "int"; "char"; "short"; 
                        "long"; "true"; "false"]

let type_of_string = function
    "int" -> Integer
  | "char" -> Char
  | "short" -> Short
  | "long" -> Long
  
let cond_no = ref 0

(* Our set of parsers *)
let rec parse_instruction = parser
  | [< lval = parse_lvalue; 'Kwd "="; rval = parse_rvalue >] ->
      Assign (lval, rval)
  | [< 'Kwd "then"; 'Kwd ":"; 'Int bid; cond = parse_condition >] ->
      incr cond_no;
      Branch (true, bid, cond)
  | [< 'Kwd "else"; 'Kwd ":"; 'Int bid; cond = parse_condition >] ->
      incr cond_no; 
      Branch (false, bid, cond)


and parse_condition = parser
  | [< arg1 = parse_lvalue; rop = parse_rop; arg2 = parse_lvalue >] ->
      Cond (rop, arg1, arg2)

and parse_value = parser
  | [< 'Kwd "_" >] ->
      None
  | [< 'Int n >] ->
      Some n

and parse_addr = parser
  | [< 'Int n >] -> n

and parse_lvalue = parser
  | [< 'Kwd "("; lval = parse_lvalue_aux >] ->
      lval

and parse_lvalue_aux = parser
  | [< addr = parse_addr; 'Kwd ","; value = parse_value; 'Kwd ")" >] ->
      (addr, value) 

and parse_rvalue = parser
  | [< 'Kwd "("; v = parser 
                 | [< cast = parse_typ; 'Kwd "("; 'Ident symvar; 'Kwd ","; 
                      'Kwd "_"; 'Kwd ")" >] ->
                     Symbvalue (cast, symvar)
                 | [< lval1 = parse_lvalue_aux; lv = parse_rvalue_aux lval1 >] ->
                     lv >] -> v
  | [< 'Kwd "-"; lval = parse_lvalue >] ->
      Neg lval

and parse_rvalue_aux arg1 = parser
  | [< op = parse_bop; arg2 = parse_lvalue >] ->
      Bop (op, arg1, arg2)
  | [< >] -> Lvalue arg1

and parse_typ = parser
  | [< 'Kwd typ; 'Kwd ")" >] ->
      type_of_string typ

and parse_bop = parser
  | [< 'Kwd "+" >] ->
      Plus
  | [< 'Kwd "-" >] ->
      Minus
  | [< 'Kwd "*" >] ->
      Mult
  | [< 'Ident op >] ->
      Unintepreted

and parse_rop = parser 
  | [< 'Kwd "==" >] -> EQ
  | [< 'Kwd "!=" >] -> NEQ
  | [< 'Kwd "<=" >] -> LE
  | [< 'Kwd "<" >] -> LT
  | [< 'Kwd ">=" >] -> GE
  | [< 'Kwd ">" >] -> GT

let parse in_chan =
  let rec read_file acc =
    match try Some (input_line in_chan) with End_of_file -> None with
        None -> List.rev acc
      | Some line -> read_file (line :: acc)
  in
  let text = read_file [] in
  let instr_lst = 
    List.map (fun line -> 
                parse_instruction @@ lexer @@ Stream.of_string line) text
  in
    instr_lst



