
module FPL = struct
end
let () =
  let open FPL in
  let open Context in
  let f = QF (Exactly (2, Px (EQ 1))) in
  let compiled = formula_to_hoas f in
  print_endline (HOAS.Show.show_formula compiled);
  let ctx = {vars=[1; 1; 2]; current_state=0} in
  let res = HOAS.eval_formula ctx compiled in
  Printf.printf "%b\n" res