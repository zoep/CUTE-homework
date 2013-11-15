open Types

module Stp = OcamlSTP;;

let ctx = Stp.make_context ();;

let width = 64 

let opOfBop = function
  | Plus -> Stp.bv_add ctx
  | Minus -> Stp.bv_sub ctx
  | Mult -> Stp.bv_mul ctx
  
let opOfRop = function
  | LT -> Stp.bv_signed_lt ctx
  | LE -> Stp.bv_signed_le ctx
  | GT -> Stp.bv_signed_gt ctx
  | GE -> Stp.bv_signed_ge ctx
  | EQ -> Stp.bv_eq ctx
  | NEQ -> (fun x y -> Stp.bool_not ctx (Stp.bv_eq ctx x y))

let rec bvOfSymExpr = function
    Symvar id -> 
    Stp.bv_var ctx id width
  | Symop (bop, symExpr1, symExpr2) ->
    let v1 = bvOfSymExpr symExpr1 in
    let v2 = bvOfSymExpr symExpr2 in
      opOfBop bop v1 v2 
  | Concrete i ->
    Stp.bv_of_int ctx width i 

let exprOfPredicate = function
	| Predicate (rop, symExpr1, symExpr2) ->
		 let v1 = bvOfSymExpr symExpr1 in
		 let v2 = bvOfSymExpr symExpr2 in
		 let op = opOfRop rop in
		  op v1 v2
  | Constant true -> Stp.bool_true ctx
  | Constant _ -> Stp.bool_false ctx

let minMaxType = function
  | Integer -> 
      (Stp.bv_of_int ctx width 32767, Stp.bv_of_int ctx width (-32767))
  | UInt -> 
      (Stp.bv_of_int ctx width 65535, Stp.bv_of_int ctx width 0)
  | Char -> 
      (Stp.bv_of_int ctx width 127, Stp.bv_of_int ctx width (-127))
  | UChar -> 
      (Stp.bv_of_int ctx width 255, Stp.bv_of_int ctx width 0)
  | Short -> 
      (Stp.bv_of_int ctx width 32767, Stp.bv_of_int ctx width (-32767))
  | UShort ->
      (Stp.bv_of_int ctx width 65535, Stp.bv_of_int ctx width 0)
  | Long ->
      (Stp.bv_of_int ctx width 2147483647, Stp.bv_of_int ctx width (-2147483647))
  | ULong -> 
      (Stp.bv_of_int ctx width 2147483647, Stp.bv_of_int ctx width 0)
  | LLong | ULLong -> failwith "LLong and ULLong not currently supported"

let output_solution solution out_file =
  Printf.fprintf out_file "sat\n";
  List.iter (fun (x, y) ->
    Printf.fprintf out_file "(= %s %d)\n" x y) solution

let solve j path_c symVars = 
  let expr = List.fold_left 
    (fun acc (s, ty) -> 
      let v = Stp.bv_var ctx s width in
      let (max, min) = minMaxType ty in
      let min_expr = Stp.bv_signed_ge ctx v min in
      let max_expr = Stp.bv_signed_le ctx v max in
      let c = Stp.bool_and ctx max_expr min_expr in
        Stp.bool_and ctx acc c) (Stp.bool_true ctx) symVars
  in
  let rec add_path_c e i = match i with 
    | -1 -> e
    | i -> 
      let c = exprOfPredicate path_c.(i) in
        add_path_c (Stp.bool_and ctx c e) (i-1)
  in
  let constrs = add_path_c expr j in 
  let neg_constrs = Stp.bool_not ctx constrs in
    match Stp.vc_query ctx neg_constrs with 
      | Stp.Valid -> 
        None 
      | Stp.Invalid ->
        let input = List.map (fun (s, _) ->
          let v = Stp.bv_var ctx s width in 
          let const = match Stp.vc_get_counterexample ctx v with
            | Some v -> v
            | None -> failwith "Impossible to reach here"
          in 
          let int_const = Stp.to_int ctx const in
            (s, int_const)) symVars
        in 
          Some input
      | Stp.Undecided -> 
        None 
  	                  
  	                             
  	                             
