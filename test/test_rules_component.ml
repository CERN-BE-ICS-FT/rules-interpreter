open Rules_component

let () =
  (* let p1 = AndPF [Px (PredEQ 3); Px (PredNEQ 3)] in
  let p2 = p1 in
  let f = AndF [QF (Forall p1); QF (Exists p2)] in *)
  let c = {
    vars = [1; 2; 3];
    currentState = 10;
  } in
  Printf.printf "%s\n" (show_context c)