open Grpc_lwt
open Lwt.Syntax

let init_connection address port =
  let* addresses =
    Lwt_unix.getaddrinfo address (string_of_int port)
      [ Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  let error_handler _ = print_endline "error" in
  H2_lwt_unix.Client.create_connection ~error_handler socket

let compute_root_node_rpc ~connection ~children_states ~current_state =
  let open Ocaml_protoc_plugin in
  let open Rules_grpc in 
  let req = RulesService.ComputeRootNode.Request.make 
              ~childrenStates:children_states 
              ~currentState:current_state
              () 
  in
  let encode, decode = Service.make_client_functions RulesService.computeRootNode in
  let enc = encode req |> Writer.contents in
  Client.call 
    ~service:"rules_service" 
    ~rpc:"ComputeRootNode"
    ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary enc ~f:(fun decoder ->
           let+ decoder = decoder in
           match decoder with
           | Some decoder -> (
               Reader.create decoder |> decode |> function
               | Ok v -> v
               | Error e ->
                   failwith
                     (Printf.sprintf "Could not decode request: %s"
                        (Result.show_error e)))
           | None -> RulesService.ComputeRootNode.Response.make ()))
    ()

let set_rules_rpc ~connection ~rules =
  let open Ocaml_protoc_plugin in
  let open Rules_grpc in 
  let req = RulesService.SetRules.Request.make ~rules () in 
  let encode, decode = Service.make_client_functions RulesService.setRules in
  let enc = encode req |> Writer.contents in
  Client.call 
    ~service:"rules_service" 
    ~rpc:"SetRules"
    ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary enc ~f:(fun decoder ->
           let+ decoder = decoder in
           match decoder with
           | Some decoder -> (
               Reader.create decoder |> decode |> function
               | Ok v -> v
               | Error e ->
                   failwith
                     (Printf.sprintf "Could not decode request: %s"
                        (Result.show_error e)))
           | None -> RulesService.SetRules.Response.make ()))
    ()

let handle_result = function
  | Ok (res, _) -> 
      Printf.printf "Result: %d" res
  | Error x -> 
      let open H2 in
      print_endline (Status.to_string x); 
      print_endline "an error occurred"

let () =
  let port = 8080 in
  let address = "localhost" in
  Lwt_main.run
    (let* connection = init_connection address port in
    let op = if Array.length Sys.argv > 1 then Sys.argv.(1) 
             else failwith "Usage: ./client <mode> ..." 
    in
    match op with
    | "root_node" ->
        if Array.length Sys.argv != 4 then failwith "Usage: ./client root_node <children_states> <current_state>"
        else
          let children_states_str = Sys.argv.(2) in
          let children_states = List.map int_of_string (String.split_on_char ',' children_states_str) in
          let current_state = int_of_string Sys.argv.(3) in
          let+ res = compute_root_node_rpc ~connection ~children_states ~current_state in
          handle_result res
    | "set_rules" ->
        if Array.length Sys.argv != 3 then failwith "Usage: ./client set_rules <rules_file>"
        else
          let rules_file = Sys.argv.(2) in 
          let rules = In_channel.input_all (In_channel.open_text rules_file) in
          let+ res = set_rules_rpc ~connection ~rules in
          handle_result res
    | _ -> failwith "Usage: ./client [root_node|set_rules]";
    )