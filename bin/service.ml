open Grpc_lwt
open Lwt.Syntax
open Ocaml_protoc_plugin
open Rules_grpc

let compute_root_node (request:ComputeRootNodeRequest.t) = 
  request.currentState

let set_rules (request:SetRulesRequest.t) = 
  let rules = request in
  if rules = "hello" then 69 else 96

(* Decodes a grpc request into actual data *)
let decode_request decode buffer =
  Reader.create buffer |> decode |> function
  | Ok v -> v
  | Error e ->
      failwith
        (Printf.sprintf "Could not decode request: %s" (Result.show_error e))

(* Returns an encoded reply *)
let make_reply encoded_reply = 
  Lwt.return (Grpc.Status.(v OK), Some (encoded_reply |> Writer.contents))

(* RPC for compute root node *)
let compute_root_node_rpc = function buffer ->
  let decode, encode = Service.make_service_functions RulesService.computeRootNode in
  let request = decode_request decode buffer in
  let result = compute_root_node request in
  let reply = RulesService.ComputeRootNode.Response.make ~result () in
  make_reply (encode reply)

(* RPC for set rules *)
let set_rules_rpc = function buffer ->
  let decode, encode = Service.make_service_functions RulesService.setRules in
  let request = decode_request decode buffer in
  let statusCode = set_rules request in
  let reply = RulesService.SetRules.Response.make ~statusCode () in
  make_reply (encode reply)

let rules_service =
  Server.Service.(
    v () |> add_rpc ~name:"ComputeRootNode" ~rpc:(Unary compute_root_node_rpc)
         |> add_rpc ~name:"SetRules" ~rpc:(Unary set_rules_rpc)
         |> handle_request)

let server =
  Server.(
    v () |> add_service ~name:"rules_service" ~service:rules_service)

let () =
  let port = 8080 in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
      let server =
        H2_lwt_unix.Server.create_connection_handler ?config:None
          ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
          ~error_handler:(fun _ ?request:_ _ _ ->
            print_endline "an error occurred")
      in
      let+ _server =
        Lwt_io.establish_server_with_client_socket listen_address server
      in
      Printf.printf "Listening on port %i for grpc requests\n" port;
      print_endline "");

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever