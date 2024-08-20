type predicate = 
  | EQ
  | NEQ
  | GT
  | LT

type 'a var =
  | Val of 'a
  | C

type 'a domain =
  | Set of 'a list
  | Whole

type 'a formula =
  | And of 'a formula list
  | Or of 'a formula list
  | Implies of 'a formula * 'a formula
  | Forall of 'a domain * ('a var -> 'a formula)
  | Exists of 'a domain * ('a var -> 'a formula)
  | ForallSet of ('a domain -> 'a formula)
  | ExistsSet of ('a domain -> 'a formula)
  | Not of 'a formula
  | P of 'a var * predicate * 'a var

type 'a prop = {
  formula : 'a formula;
  resulting_state : 'a
}

type 'a rules_list = RulesList of ('a prop) list 

(* Show submodule *)
module Show = struct
  let show_pred = function
    | (EQ)  -> " == "
    | (NEQ) -> " != "
    | (LT)  -> " < " 
    | (GT)  -> " > " 

  let rec intercalate element = function
    | []      -> []
    | [x]     -> [x]
    | x :: xs -> x :: element :: intercalate element xs

  let name_var offset = 
    if offset = -1 then "C" else
    if offset >= 1000 then  
      let base = 'a' in
      String.make 1 (Char.chr (Char.code base + offset - 1000))
    else
      string_of_int offset

  let unval v =
    match v with
    | Val x -> x 
    | C -> -1 

  let get_domain_name s =
    match s with
    | Set s ->
        (match s with
        | (x::_) -> name_var x 
        | [] -> "_")
    | Whole -> ""

  let show_formula (f:int formula) : string = 
    let rec show_formula_with level f =
      match f with
      | (And fs) ->
          let showed = List.map (show_formula_with level) fs in
          let anded = intercalate " & " showed in
          "(" ^ String.concat "" anded ^ ")"
      | (Or fs)  ->
          let showed = List.map (show_formula_with level) fs in
          let ored = intercalate " | " showed in
          "(" ^ String.concat "" ored ^ ")"
      | (Implies (f1,f2)) ->
          show_formula_with level f1 ^ " => " ^ show_formula_with level f2
      | (Forall (s,f)) -> 
          let id = name_var level in
          let domain = get_domain_name s in
          let sub = show_formula_with (level + 1) (f (Val level)) in
          "(∀" ^ id ^ " ∈ " ^ domain ^ ". " ^ sub ^ ")"
      | (Exists (s,f)) ->
          let id = name_var level in
          let domain = get_domain_name s in
          let sub = show_formula_with (level + 1) (f (Val level)) in
          "(∃" ^ id ^ " ∈ " ^ domain ^ ". " ^ sub ^ ")"
      | (ForallSet f) ->
          let domain = name_var level in
          let sub = show_formula_with (level + 1) (f (Set [level])) in
          "(∀" ^ domain ^ " : set. " ^ sub ^ ")"
      | (ExistsSet f) ->
          let domain = name_var level in
          let sub = show_formula_with (level + 1) (f (Set [level])) in
          "(∃" ^ domain ^ " : set. " ^ sub ^ ")"
      | (P (x,p,y)) ->
          name_var (unval x) ^ show_pred p ^ name_var (unval y)
      | Not f ->
          "!(" ^ show_formula_with level f ^ ")"
           
    in 
    show_formula_with 1000 f
end
  
let eval_pred p : int -> int -> bool =
  match p with
  | EQ  -> (=)
  | NEQ -> (!=)
  | LT  -> (<)
  | GT  -> (>)

let choose_var x c =
  match x with
  | Val x' -> x'
  | C -> c 

open List

let rec powerset = function
  | [] -> [[]]
  | (x :: xs) -> 
      let p = powerset xs in 
      flatten [List.map (fun ys -> x :: ys) p; p]

let sizeof set =
  match set with
  | Set s -> Val (length s)
  | Whole -> Val (-1)

let choose_domain set whole_domain =
  match set with
  | Whole -> whole_domain
  | Set s  -> s

let rec eval_formula (c : 'a Context.context) = function
  | P (x, p, y) -> eval_pred p (choose_var x c.current_state) (choose_var y c.current_state)
  | Forall (s,k) -> for_all (fun v -> eval_formula c (k (Val v))) (choose_domain s c.vars)
  | Exists (s,k) -> exists (fun v -> eval_formula c (k (Val v))) (choose_domain s c.vars)
  | ForallSet k -> for_all (fun s -> eval_formula c (k (Set s))) (powerset c.vars) 
  | ExistsSet k -> exists (fun s -> eval_formula c (k (Set s))) (powerset c.vars) 
  | And fs -> for_all (fun f -> eval_formula c f) fs
  | Or fs -> exists (fun f -> eval_formula c f) fs
  | Implies (f1,f2) -> eval_formula c f2 || not (eval_formula c f1)
  | Not f -> not (eval_formula c f)

let show_list to_string l =  
  let rec show_elems = function
    | []      -> ""
    | [x]     -> to_string x
    | x :: xs -> to_string x ^ ", " ^ show_elems xs
  in
  "[" ^ show_elems l ^ "]"