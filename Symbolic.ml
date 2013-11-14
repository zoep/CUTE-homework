
open Parser 
  
type symExpr =
    | Symvar of symvar
    | Symop of bop * symExpr * symExpr 
    | Symneg of symvar
    | Concrete of int

(* Holds (address, expr) *)
let env = Hashtbl.create 101

let execute_symbolic instr_node = 
  match instr_node.instr with
    | Assign ((_, _), Lvalue (0, _)) -> ()
    | Assign ((m1, _), Lvalue (m2, _)) ->
        (match (try Some (Hashtbl.find env m2) with Not_found -> None) with 
          | Some v -> Hashtbl.add env m1 v
          | None -> Hashtbl.remove env m1)
    | Assign ((m1, _), Neg (0, _)) -> ()
    | Assign ((m1, _), Neg (m2, _)) -> ()
        (match (try Some (Hashtbl.find env m2) with Not_found -> None) with 
          | Some v -> Hashtbl.add env m1 (Symneg v)
          | None -> Hashtbl.remove env m1)
    | Assign ((m1, _), Bop (bop, (m2, vopt2), (m3, vopt3))) when bop = Mult ->
        (match (try Some (Hashtbl.find env m2) with Not_found -> None,
                try Some (Hashtbl.find env m3) with Not_found -> None) with 
           | Some v2, Some v3 -> 
               (match vopt2, vopt3 with
                  | Some cv2, _ -> 
                      Hashtbl.add env m1 (Symop (bop, Concrete cv2, v3) 
                  | _, Some cv3 -> 
                      Hashtbl.add env m1 (Symop (bop, v2, Concrete cv3)
                  | _ -> failwith "Cannot proceed with no concrete value)" 
           | Some v2, None ->
               let cv3 = match vopt3 with 
                   Some v -> Concrete v
                 | None -> failwith "Cannot proceed with no concrete value"
               in 
                 Hashtbl.add env m1 (Symop (bop, v2, cv3)
           | None, Some v3 ->
               let cv2 = match vopt2 with 
                   Some v -> Concrete v
                 | None -> failwith "Cannot proceed with no concrete value"
               in 
                 Hashtbl.add env m1 (Symop (bop, cv2, v3)
           | None, None -> Hashtbl.remove env m1)
    | Assign ((m1, _), Bop (bop, (m2, vopt2), (m3, vopt3))) ->
        (match (try Some (Hashtbl.find env m2) with Not_found -> None,
                try Some (Hashtbl.find env m3) with Not_found -> None) with 
           | Some v2, Some v3 -> 
               Hashtbl.add env m1 (Symop (bop, v2, v3) 
           | Some v2, None ->
               let cv3 = match vopt3 with 
                   Some v -> Concrete v
                 | None -> failwith "Cannot proceed with no concrete value"
               in 
                 Hashtbl.add env m1 (Symop (bop, v2, cv3)
           | None, Some v3 ->
               let cv2 = match vopt2 with 
                   Some v -> Concrete v
                 | None -> failwith "Cannot proceed with no concrete value"
               in 
                 Hashtbl.add env m1 (Symop (bop, cv2, v3)
           | None, None -> Hashtbl.remove env m1)
   | Assign ((m1, _), Symbvalue (ty, symvar)) ->
       Hashtbl.add env m1 (Symvar symvar) 
   | 
