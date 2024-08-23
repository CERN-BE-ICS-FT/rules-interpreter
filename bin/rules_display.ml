let () =
  (* Argv validation *)
  let argc = Array.length Sys.argv in
  if argc != 2 then
    print_endline "Usage: ./test.exe [rules]"
  else
  
  let rules_str = Sys.argv.(1) in
  let rules_res = FPL.Parser.parse_rules rules_str in
  match rules_res with
  | Ok rules -> print_endline (FPL.Show.show_rules rules)
  | Error e ->  print_endline ("Parse error: " ^ e)