type target = C

type 'a predicate = 
  | PredEQ of 'a
  | PredNEQ of 'a
  | PredGT of 'a 
  | PredLT of 'a

type 'a propFormula =
  | AndPF of (('a propFormula) list)
  | OrPF of (('a propFormula) list)
  | Px of ('a predicate)

type 'a quantFormula =
  | Forall of ('a propFormula)
  | Exists of ('a propFormula)

type 'a formula =
  | AndF of (('a formula) list)
  | OrF of (('a formula) list)
  | QF of ('a quantFormula)
  | Pt of (target * ('a predicate))

type 'a prop = {
  formula : 'a formula;
  resultingState : 'a
}

type 'a rulesList = ('a prop) list 

(* Show instances *)
let show_target = function
  | C -> "C"

let show_pred = function
  | (PredEQ x)  -> " == " ^ (string_of_int x)
  | (PredNEQ x) -> " != " ^ (string_of_int x)
  | (PredLT x)  -> " < "  ^ (string_of_int x)
  | (PredGT x)  -> " > "  ^ (string_of_int x)

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

(* Context *)
type 'a context = {
  vars: 'a list;
  currentState: 'a;
} 

(* Context show instances *)
let show_list l =  
  let rec show_elems = function
    | []      -> ""
    | [x]     -> x
    | x :: xs -> x ^ ", " ^ show_elems xs
  in
  "[" ^ show_elems l ^ "]"

let show_context c = 
  let vars = "VARS: " ^ show_list (List.map string_of_int c.vars) in
  let state = "CURRENT STATE: " ^ string_of_int (c.currentState) in
  vars ^ "\n" ^ state

(* Interpreter *)
let get_target t c = 
  match t with
    | C -> c.currentState

let eval_pred = function
  | (PredEQ x)  -> (=) x
  | (PredNEQ x) -> (!=) x
  | (PredLT x)  -> (>) x
  | (PredGT x)  -> (<) x

let rec eval_prop_formula = function
  | (AndPF fs) -> fun x ->
      List.for_all (fun f -> (eval_prop_formula f) x) fs
  | (OrPF fs) -> fun x ->
      List.exists (fun f -> (eval_prop_formula f) x) fs 
  | (Px p) -> eval_pred p

let eval_quant_formula c = function
  | (Forall pf) -> List.for_all (eval_prop_formula pf) c.vars
  | (Exists pf) -> List.exists (eval_prop_formula pf) c.vars

let rec eval_formula c = function
  | (AndF fs)  -> List.for_all (eval_formula c) fs
  | (OrF fs)   -> List.exists  (eval_formula c) fs
  | (QF q)     -> eval_quant_formula c q
  | (Pt (t,p)) -> eval_pred p (get_target t c)

let eval_prop c prop =
  if eval_formula c (prop.formula) then 
    Some (prop.resultingState)
  else
    None

(* Applies all the rules to a given context *)
let apply_rules rulesList context =
  let results = List.map (eval_prop context) rulesList in
  List.hd (List.flatten (List.map Option.to_list results))