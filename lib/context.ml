type 'a context = {
  vars: 'a list;
  current_state: 'a;
} 

let show_list l =  
  let rec show_elems = function
    | []      -> ""
    | [x]     -> x
    | x :: xs -> x ^ ", " ^ show_elems xs
  in
  "[" ^ show_elems l ^ "]"

let show_context c = 
  let vars = "VARS: " ^ show_list (List.map string_of_int c.vars) in
  let state = "CURRENT STATE: " ^ string_of_int (c.current_state) in
  vars ^ "\n" ^ state
  
let parse_context input =
  let open Yojson.Basic.Util in
  let json = Yojson.Basic.from_string input in
  let vars = json |> member "vars" 
                  |> to_list 
                  |> List.map to_int
  in
  let current_state = json |> member "currentState" |> to_int in
  { vars; current_state }