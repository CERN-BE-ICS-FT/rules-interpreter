(* Note: FPL stands for Fancy Propositional Logic  
- it has wild quantifiers, but it reduces to propositional logic *)

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
  | AtLeast of int * 'a prop_formula
  | AtMost of int * 'a prop_formula
  | Exactly of int  * 'a prop_formula

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

  let show_quant_formula qf = 
    match qf with
    | (Forall pf) -> "(∀x. " ^ show_prop_formula pf ^ ")"
    | (Exists pf) -> "(∃x. " ^ show_prop_formula pf ^ ")"
    | (Exactly (n,pf)) -> "(∃=" ^ string_of_int n ^ "x. " ^ show_prop_formula pf ^ ")"
    | (AtLeast (n,pf)) -> "(∃>" ^ string_of_int n ^ "x. " ^ show_prop_formula pf ^ ")"
    | (AtMost (n,pf)) -> "(∃<" ^ string_of_int n ^ "x. " ^ show_prop_formula pf ^ ")"

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

module Compiler = struct
  let pred_to_hoas = function
    | EQ x -> (HOAS.EQ, x)
    | NEQ x -> (HOAS.NEQ, x)
    | GT x -> (HOAS.GT, x)
    | LT x -> (HOAS.LT, x)

  let rec prop_formula_to_hoas (f : 'a prop_formula) x = 
    match f with
    | AndPF fs -> HOAS.And (List.map (fun f -> prop_formula_to_hoas f x) fs)
    | OrPF fs  -> HOAS.Or (List.map (fun f -> prop_formula_to_hoas f x) fs)
    | Px p     -> 
        let (pred, value) = pred_to_hoas p in 
        HOAS.P (x, pred, HOAS.Val value)

  let rec qf_to_hoas (qf: 'a quant_formula) =
    let open HOAS in
    match qf with
    | Forall f -> Forall (Whole, prop_formula_to_hoas f)
    | Exists f -> Exists (Whole, prop_formula_to_hoas f)
    | AtLeast (n,f) -> 
        let set_predicate s = P (sizeof s, EQ, Val n) in
        ExistsSet (fun set -> And [set_predicate set; Forall(set, prop_formula_to_hoas f)])
    | AtMost (n,f) -> 
        let set_predicate s = P (sizeof s, EQ, Val (n + 1)) in
        ForallSet (fun set -> Implies (set_predicate set, Exists(set, fun x -> Not (prop_formula_to_hoas f x))))
    | Exactly (n,f) ->
        formula_to_hoas (AndF [QF (AtLeast (n,f)); QF (AtMost (n,f))])

  and formula_to_hoas f =
    match f with
    | AndF fs -> HOAS.And (List.map formula_to_hoas fs)
    | OrF  fs -> HOAS.Or (List.map formula_to_hoas fs)
    | QF   qf -> qf_to_hoas qf 
    | Pt (_,p)-> 
        let (pred,value) = pred_to_hoas p in
        HOAS.P (HOAS.C, pred, HOAS.Val value)
end

(* Interpreter submodule *)
module Interpreter : sig
  val eval_rules : int rules_list -> int Context.context -> int option
end = struct
  open List

  (* Unpack all 'some' values in a list *)
  let only_some xs =
    map Option.get (filter Option.is_some xs)

  let eval_formula c f =
    let compiled = Compiler.formula_to_hoas f in
    HOAS.eval_formula c compiled

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
  val parse_rules : string -> (int rules_list, string) result
end = struct
  open Yojson.Basic.Util

  (* Define monadic let notation for 'result' *)
  let return v = Ok v
  let (let*) o f =
    match o with
    | Ok v -> (f v)
    | Error e -> Error e
  
  let fold_res xs = 
    List.fold_right (fun v acc ->
      let* a = acc in
      let* x = v in
      return (x :: a)
    ) 
    xs (return [])

  let parse_target t =
    match t with
      | "C" -> return C
      | _   -> Error "Could not parse target."

  let parse_pred p =
    let op = p |> member "op" |> to_string in
    let arg = p |> member "arg" |> to_int in
    match op with
      | "==" -> return (EQ arg)
      | "!=" -> return (NEQ arg)
      | ">"  -> return (GT arg)
      | "<"  -> return (LT arg)
      | _    -> Error "Could not parse predicate." 

  let rec parse_prop_formula f =
    let op_option = f |> member "op" |> to_string_option in
    match op_option with
      | Some op -> 
          let* fs = f |> member "formulas" 
                     |> to_list
                     |> List.map parse_prop_formula
                     |> fold_res
          in
          (match op with
            | "AND" -> return (AndPF fs)
            | "OR"  -> return (OrPF fs)
            | _     -> Error "Could not parse prop formula logical op.")
      | None ->
          let* pred = f |> member "predicate" |> parse_pred in
          return (Px (pred))

  let parse_quant_formula f =
    let quantifier = f |> member "quantifier" |> to_string in
    let* prop_formula = f |> member "propFormula" |> parse_prop_formula in
    match quantifier with
      | "forall" -> return (Forall prop_formula)
      | "exists" -> return (Exists prop_formula)
      | _        -> Error "Could not parse quantifier."

  let rec parse_formula f = 
    let op_option = f |> member "op" |> to_string_option in
    match op_option with
      | Some op -> 
          let* fs = f |> member "formulas"
                     |> to_list 
                     |> List.map (fun f' -> parse_formula f') 
                     |> fold_res
          in
          (match op with
            | "AND" -> return (AndF fs)
            | "OR"  -> return (OrF fs)
            | _     -> Error "Could not parse formula logical op.")
      | None ->
          let pred_option = f |> member "predicate" |> to_option (fun x -> x) in
          match pred_option with
            | Some pred ->
                let t = f |> member "target" |> to_string in
                let* target = parse_target t in
                let* predicate = parse_pred pred in
                return (Pt (target, predicate))
            | None -> 
                let* qf = parse_quant_formula f in
                return (QF qf)

  let parse_prop p =
    let* formula = p |> member "formula" |> parse_formula in
    let resulting_state = p |> member "resultingState" |> to_int in
    return { formula; resulting_state }


  let _parse_rules input = 
    let json = Yojson.Basic.from_string input in
    let* rules = json |> member "propositions" 
                     |> to_list 
                     |> List.map parse_prop
                     |> fold_res
    in
    return (RulesList rules)

  let parse_rules input =
    try _parse_rules input with
      _ -> Error "An error occured while parsing JSON."
end

(*
(>>=) :: Either a String -> (a -> Either b String) -> Either b String
(>>=) val f = case val of
  Left x -> Left (f x)
  Right s -> Right s

f :: [Either Int String] -> Either [Int] String
f xs = foldl (\acc v -> acc >>= (\a -> v >>= \v' -> Left (acc ++ v'))) (Left []) xs
*)