let main = 
  let in_chan = open_in "trace" in
  let parsed = Parser.parse in_chan in
  let stack = Constraints.make_stack () in
  let path_c = Constraints.make_path_c () in
  let () = 
    try     
      let stack_file = open_in "stack" in
        Constraints.init_stack stack_file stack;
        close_in stack_file;
    with 
        Sys_error _ -> ()
  in
  let output_file = open_out "input" in
  let stack_file = open_out "stack" in
  let () = List.iter (Symbolic.execute_symbolic path_c stack) parsed in
  let symVars = Symbolic.get_symVars () in
  match 
    (try 
       Some (Constraints.solve_path_constraint path_c stack !Parser.cond_no symVars)
     with Constraints.Completed -> None) 
  with 
    | Some (solution, j) ->
        Constraints.output_stack stack j stack_file;
        Solver.output_solution solution output_file;
        exit 0
    | None -> 
        exit 1

