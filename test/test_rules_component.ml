open Rules_component

let () =
  let c = {
    vars = [2; 2; 0];
    current_state = 1;
  } in
  let rules = parse_rules "rules.json" in
  print_endline (show_rules rules);
  print_endline (show_context c);

  let res = apply_rules rules c in
  match res with
    | Some x -> Printf.printf "%d\n" x 
    | None -> print_endline "Nothing"