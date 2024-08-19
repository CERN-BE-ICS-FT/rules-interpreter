let () =
  (* Argv validation *)
  let argc = Array.length Sys.argv in
  if argc != 3 then
    print_endline "Usage: ./test.exe [rules] [context]"
  else
  
  (* Read rules and context as strings *)
  let rules_str = Sys.argv.(1) in
  let context_str = Sys.argv.(2) in
  
  (* Parse rules and context *)
  let rules_res = Rules.Parser.parse_rules rules_str in
  match rules_res with
  | Error e -> print_endline e
  | Ok rules ->
      let context = Context.parse_context context_str in
      (* Apply rules *)
      let res_opt = Rules.Interpreter.eval_rules rules context in
      match res_opt with
        | Some res -> Printf.printf "%d\n" res
        | None -> Printf.printf "%d\n" context.current_state