let read_file path =
  In_channel.input_all (In_channel.open_text path)

let () =
  (* Argv validation *)
  let argc = Array.length Sys.argv in
  if argc != 3 then
    print_endline "Usage: ./test.exe <rules.json> <context.json>"
  else
  
  (* Read rules and context as strings *)
  let rules_str = read_file (Sys.argv.(1)) in
  let context_str = read_file (Sys.argv.(2)) in
  
  (* Parse rules and context *)
  let rules_res = FPL.Parser.parse_rules rules_str in
  match rules_res with
  | Error e -> print_endline e
  | Ok rules ->
      let context = Context.parse_context context_str in

      (* Show rules and context *)
      print_endline (FPL.Show.show_rules rules);
      print_endline (Context.show_context context);

      (* Apply rules *)
      let res_opt = FPL.Interpreter.eval_rules rules context in
      match res_opt with
        | Some res -> Printf.printf "%d\n" res
        | None -> print_endline "No change."