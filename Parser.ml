open Genlex

exception SymValue

type instr_node = {
  instr : instruction
}

and instruction = 
  | Assign of lvalue * rvalue 
  | Branch of taken * id * condition  

and taken = bool                            

and id = int

and lvalue = address * (value option) 

and rvalue = 
  | Bop of bop * lvalue * lvalue
  | Neg of lvalue
  | Lvalue of lvalue
  | Symbvalue of typ * symvar
                         
and symvar = string

and condition = 
  | Const of bool 
  | Cond of rop * lvalue * lvalue

and bop = Plus | Minus | Mult | Unintepreted

and rop = LT | LE | GT | GE | EQ | NEQ 

and typ = 
  | Integer | UInt | Char | UChar  
  | Short | UShort | Long | ULong
  | LLong | ULLong 

and address = int

and value = int

let lexer = make_lexer ["("; ")"; "+"; "-"; "*"; 
                        "then";":"; "else"; ","; "="; 
                        "=="; "<"; ">"; "<="; "_"; "!=";
                        ">="; "int"; "char"; "short"; 
                        "long"; "true"; "false"]

(** Checks whether a memory address is a symbolic variable or not *) 
let isSymbolic str = 
  let len = String.length str in
  let rec aux i = 
    if (i = len) then false
    else
      match str.[i] with
        | '0'..'9' -> aux (i+1)
        | _ -> true
  in
    aux 0

let type_of_string = function
    "int" -> Integer
  | "char" -> Char
  | "short" -> Short
  | "long" -> Long

(* Our set of parsers *)
let rec parse_instruction = parser
  | [< lval = parse_lvalue; 'Kwd "="; rval = parse_rvalue >] ->
      {instr = Assign (lval, rval)}
  | [< 'Kwd "then"; 'Kwd ":"; 'Int bid; cond = parse_condition >] ->
      {instr = Branch (true, bid, cond)}
  | [< 'Kwd "else"; 'Kwd ":"; 'Int bid; cond = parse_condition >] ->
      {instr = Branch (false, bid, cond)}


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

(*
(* Our set of parsers *)
let rec parse_instruction = parser
  | [< 'Kwd "("; assign = parse_assign >] -> parse_assign 
  | [< 'Kwd "then"; 'Kwd ":"; 'Int id; condition = parse_condition >] ->
      Branch (true, id, condition)
  | [< 'Kwd "else"; 'Kwd ":"; 'Int id; condition = parse_condition >] -> 
      Branch (false, id, condition)

and parse_assign = parser
  | [< lhs = parse_lvalue; 'Kwd ")"; 'Kwd "="; rhs = parse_rvalue; >] ->
      Assign (lhs, rhs)

and parse_lvalue = parser
  | [< mem = parse_addr; 'Kwd ","; value = parse_value >] ->
      (mem, value)

and parse_rvalue = parser 
  | [< 'Kwd "("; r1 = parser 
                 | [< left = parse_lvalue; 'Kwd ")"; 
                      r2 = parser
                      | [< op = parse_bop; right = parse_lvalue >] ->
                          Bop (op, left, right) 
                      | [< >] -> Lvalue left
                    >] -> r2
                 | [< cast = parse_typ; 'Kwd "("; 
                      lvalue = parse_lvalue; 'Kwd ")" >] ->
                     Symbvalue (cast, lvalue)
  ; 'Kwd ")" >] -> r1
  | [< 'Kwd "-"; value = parse_lvalue >] ->
      Neg value

and parse_value = parser
  | [< 'Kwd "_" >] ->
      None
  | [< 'Int n >] ->
      Some n

and parse_addr = parser
  | [< 'Int n >] ->
      Concrete n
  | [< 'Ident sym_var >] -> 
      Symbolic sym_var

and parse_typ = parser
  | [< 'Kwd typ; 'Kwd ")" >] ->
      Some (type_of_string typ)

and parse_bop = parser
  | [< 'Kwd "+" >] ->
      Plus
  | [< 'Kwd "-" >] ->
      Minus
  | [< 'Kwd "*" >] ->
      Mult
  | [< 'Ident op >] ->
      Unintepreted

and parse_cond = parser 
  | [< left = parse_lvalue; rop = parse_rop; right = parse_lvalue >] ->
      Cond (rop, lvalue, rvalue)

and parse_rop = parser 
  | [< 'Kwd "==" >] -> EQ
  | [< 'Kwd "!=" >] -> NEQ
  | [< 'Kwd "<=" >] -> LEG
  | [< 'Kwd "<" >] -> LT
  | [< 'Kwd ">=" >] -> GEQ
  | [< 'Kwd ">" >] -> GE*)

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

