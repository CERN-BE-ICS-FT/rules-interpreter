open Grpc_lwt
open Lwt.Syntax
open Ocaml_protoc_plugin
open Rules.Rules

let run_rules buffer =
  let decode, encode = Service.make_service_functions RulesService.runRules in
  let request =
    Reader.create buffer |> decode |> function
    | Ok v -> v
    | Error e ->
        failwith
          (Printf.sprintf "Could not decode request: %s" (Result.show_error e))
  in
  let result =
    if request = "" then "You forgot your name!"
    else Format.sprintf "Hello, %s!" request
  in
  let reply = RulesService.RunRules.Response.make ~result () in
  Lwt.return (Grpc.Status.(v OK), Some (encode reply |> Writer.contents))

let rules_service =
  Server.Service.(
    v () |> add_rpc ~name:"RunRules" ~rpc:(Unary run_rules) |> handle_request)

let server =
  Server.(
    v () |> add_service ~name:"rules-service" ~service:rules_service)


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