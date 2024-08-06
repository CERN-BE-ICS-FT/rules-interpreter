type target = C

type 'a predicate = 
  | EQ of 'a
  | NEQ of 'a
  | GT of 'a 
  | LT of 'a

type 'a prop_formula =
  | AndPF of 'a prop_formula list
  | OrPF of 'a prop_formula list
  | Px of 'a predicate

type 'a quant_formula =
  | Forall of 'a prop_formula
  | Exists of 'a prop_formula

type 'a formula =
  | AndF of 'a formula list
  | OrF of 'a formula list
  | QF of 'a quant_formula
  | Pt of target * 'a predicate

type 'a prop = {
  formula : 'a formula;
  resulting_state : 'a
}

type 'a rules_list = RulesList of ('a prop) list 

(* Show submodule *)
module Show = struct
  let show_target = function
    | C -> "C"

  let show_pred = function
    | (EQ x)  -> " == " ^ (string_of_int x)
    | (NEQ x) -> " != " ^ (string_of_int x)
    | (LT x)  -> " < "  ^ (string_of_int x)
    | (GT x)  -> " > "  ^ (string_of_int x)

  let rec intercalate element = function
    | []      -> []
    | [x]     -> [x]
    | x :: xs -> x :: element :: intercalate element xs

  let rec show_prop_formula = function
    | (AndPF fs) -> let showed = List.map show_prop_formula fs in
                    let anded = intercalate " & " showed in
                    "(" ^ String.concat "" anded ^ ")"
    | (OrPF fs)  -> let showed = List.map show_prop_formula fs in
                    let ored = intercalate " | " showed in
                    "(" ^ String.concat "" ored ^ ")"
    | (Px pred)  -> "x" ^ show_pred pred

  let show_quant_formula = function
    | (Forall pf) -> "(∀x. " ^ show_prop_formula pf ^ ")"
    | (Exists pf) -> "(∃x. " ^ show_prop_formula pf ^ ")"

  let rec show_formula = function 
    | (AndF fs) -> let showed = List.map show_formula fs in
                   let anded = intercalate " AND " showed in
                   "[" ^ String.concat "" anded ^ "]" 
    | (OrF fs)  -> let showed = List.map show_formula fs in
                   let ored = intercalate " OR " showed in
                   "[" ^ String.concat "" ored ^ "]"
    | (QF f)    -> show_quant_formula f
    | (Pt (t, p))  -> show_target t ^ show_pred p

  let show_prop prop =
    let if_str = "IF " ^ (show_formula prop.formula) in
    let then_str = "\n  THEN " ^ (string_of_int prop.resulting_state) in
    if_str ^ then_str

  let show_rules (RulesList props) = 
    String.concat "\n" (List.map show_prop props)
end

(* Interpreter submodule *)
module Interpreter : sig
  val eval_rules : 'a rules_list -> 'a Context.context -> 'a option
end = struct
  open List
  type 'a context = 'a Context.context

  (* Unpack all 'some' values in a list *)
  let only_some xs =
    map Option.get (filter Option.is_some xs)

  let get_target t (c : 'a context) = 
    match t with
      | C -> c.current_state

  let eval_pred = function
    | (EQ x)  -> (=)  x
    | (NEQ x) -> (!=) x
    | (LT x)  -> (>)  x
    | (GT x)  -> (<)  x

  let rec eval_prop_formula = function
    | (AndPF fs) -> fun x -> for_all (fun f -> (eval_prop_formula f) x) fs
    | (OrPF fs)  -> fun x -> exists  (fun f -> (eval_prop_formula f) x) fs 
    | (Px p)     -> eval_pred p

  let eval_quant_formula (c : 'a context) = function
    | (Forall pf) -> for_all (eval_prop_formula pf) c.vars
    | (Exists pf) -> exists  (eval_prop_formula pf) c.vars

  let rec eval_formula c = function
    | (AndF fs)  -> for_all (eval_formula c) fs
    | (OrF fs)   -> exists  (eval_formula c) fs
    | (QF q)     -> eval_quant_formula c q
    | (Pt (t,p)) -> eval_pred p (get_target t c)

  let eval_prop c prop =
    if eval_formula c (prop.formula) = true then 
      Some (prop.resulting_state)
    else
      None

  let eval_rules (RulesList rules) context =
    let results = map (eval_prop context) rules in
    try Some (hd (only_some results)) with
      Failure _ -> None
end

(* Parses rules from json *)
module Parser : sig 
  val parse_rules : string -> int rules_list
end = struct
  open Yojson.Basic.Util

  let parse_target t =
    match t with
      | "C" -> C
      | _   -> failwith "Could not parse target."

  let parse_pred p =
    let op = p |> member "op" |> to_string in
    let arg = p |> member "arg" |> to_int in
    match op with
      | "==" -> EQ arg
      | "!=" -> NEQ arg
      | ">"  -> GT arg
      | "<"  -> LT arg
      | _    -> failwith "Could not parse predicate." 

  let rec parse_prop_formula f =
    let op_option = f |> member "op" |> to_string_option in
    match op_option with
      | Some op -> 
          let fs = f |> member "formulas" 
                     |> to_list
                     |> List.map parse_prop_formula
          in
          (match op with
            | "AND" -> AndPF fs
            | "OR"  -> OrPF fs
            | _     -> failwith "Could not parse prop formula logical op.")
      | None ->
          let pred = f |> member "predicate" |> parse_pred in
          Px (pred)

  let parse_quant_formula f =
    let quantifier = f |> member "quantifier" |> to_string in
    let prop_formula = f |> member "propFormula" |> parse_prop_formula in
    match quantifier with
      | "forall" -> Forall prop_formula
      | "exists" -> Exists prop_formula
      | _        -> failwith "Could not parse quantifier."


  let rec parse_formula f = 
    let op_option = f |> member "op" |> to_string_option in
    match op_option with
      | Some op -> 
          let fs = f |> member "formulas"
                     |> to_list 
                     |> List.map (fun f' -> parse_formula f') 
          in
          (match op with
            | "AND" -> AndF fs
            | "OR"  -> OrF fs
            | _     -> failwith "Could not parse formula logical op.")
      | None ->
          let pred_option = f |> member "predicate" |> to_option (fun x -> x) in
          match pred_option with
            | Some pred ->
                let t = f |> member "target" |> to_string in
                Pt (parse_target t, parse_pred pred)
            | None -> 
                QF (parse_quant_formula f) 

  let parse_prop p =
    let formula = p |> member "formula" |> parse_formula in
    let resulting_state = p |> member "resultingState" |> to_int in
    { formula; resulting_state }

  let parse_rules input = 
    let json = Yojson.Basic.from_string input in
    let rules = json |> member "propositions" 
                     |> to_list 
                     |> List.map parse_prop in
    RulesList rules
end