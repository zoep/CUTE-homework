let main = 
  let in_chan = open_in "../cute/tests/trace" in
  let parsed = Parser.parse in_chan in
  let stack_file = open_in "stack" in
  let output_file = open_out "../cute/tests/input" in
  let () = Constraints.init_stack stack_file in
  let () = close_in stack_file in
  let stack_file = open_out "stack" in
  let () = List.iter Symbolic.execute_symbolic parsed in
  match (try Some (Constraints.solve_path_constraint Constraints.cond_no)
         with Constraints.Completed -> None) with 
    | Some (solution, j) ->
        Constraints.output_stack j stack_file;
        Solver.output_solution solution output_file
    | None -> 
        exit 1

