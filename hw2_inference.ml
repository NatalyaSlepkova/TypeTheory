open Hw1;;
open Hw1_reduction;;
open Hw22_unify;;
open List;;

(** Simple type is a type name or arrow operator *)
type simp_type =
  | S_Elem of string                    (** Type: a       *)
  | S_Arrow of simp_type * simp_type    (** Arrow: a -> a *)

(** Hindley-Milner lambda is a variable, abstraction, application or let *)
type hm_lambda =
  | HM_Var of string                            (** Variable: x          *)
  | HM_Abs of string * hm_lambda                (** Abstraction: λx.f    *)
  | HM_App of hm_lambda * hm_lambda             (** Application: f x     *)
  | HM_Let of string * hm_lambda * hm_lambda    (** Let: let x = t in t' *)

(** Hindley-Milner type is a type name, arrow operator or forall quantifier *)
type hm_type =
  | HM_Elem of string                (** Type: a          *)
  | HM_Arrow of hm_type * hm_type    (** Arrow: a -> a    *)
  | HM_ForAll of string * hm_type    (** Quantifier: ∀t.t *)

module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;

let unique_type = Stream.from (fun i -> Some ("type" ^ string_of_int i));;

let unique_var = Stream.from (fun i -> Some ("var" ^ string_of_int i));;

let rec term_of_simp_type t =
  let to_term = term_of_simp_type in
  match t with
  | S_Elem v -> Var v
  | S_Arrow (a, b) -> Fun("->", [ (to_term a); (to_term b) ]);;

let equation_of_types (l, r) = (term_of_simp_type l, term_of_simp_type r);;

let new_type() = S_Elem (Stream.next unique_type)
let add_type_to_map map t = StringMap.add t (new_type()) map
let rec get_system lambda types =
  match (lambda : lambda) with
  | Var v -> ([], StringMap.find v types)
  | App (lambda1, lambda2) ->
    let (system1, t1) = get_system lambda1 types in  
    let (system2, t2) = get_system lambda2 types in
    let new_t = new_type() in
    ((concat [system1; system2; [(t1, S_Arrow(t2, new_t))] ]), new_t)
  | Abs (v, l) ->
    let new_map = add_type_to_map types v in
    let (system1, t1) = get_system l new_map in
    (system1, S_Arrow(StringMap.find v new_map, t1));;


let infer_simp_type lambda =
  let free = free_vars lambda in
  let types = List.fold_left add_type_to_map StringMap.empty free in
  let (system, t) = get_system lambda types in
  match solve_system (List.map equation_of_types system) with
  | None -> None
  | Some solution ->
    let rec simplification_term t = match t with
    | Var v -> S_Elem v
    | Fun(f, [ l; r ]) when f = "->" -> S_Arrow (simplification_term l, simplification_term r)
    | _ -> failwith "It isn't a simple type" in 
    let lambda_type_term = apply_substitution solution (term_of_simp_type t) in
    let to_type_list = List.map (fun (a, b) -> (a, simplification_term b)) in
    Some (to_type_list solution, simplification_term lambda_type_term);;   


let rec term_of_hm_type hm_type =
  match hm_type with
  | HM_Elem a  -> Var a
  | HM_Arrow (a, b) -> Fun ("->", [ (term_of_hm_type a); (term_of_hm_type b) ])
  | _ -> failwith "Forall quantifier cannot be represented as a term";;

let rec hm_type_of_term term =
  match term with
  | Var a  -> HM_Elem a
  | Fun (f, [l;r]) when f = "->" -> HM_Arrow(hm_type_of_term l, hm_type_of_term r)
  | _ -> failwith "It isn't a simple type";;

let rec apply_type_subst subst hm_type =
  match hm_type with
  | HM_Elem a when StringMap.mem a subst -> StringMap.find a subst
  | HM_Elem a -> hm_type
  | HM_Arrow (a, b) ->
    HM_Arrow (apply_type_subst subst a, apply_type_subst subst b)
  | HM_ForAll (a, b) ->
    HM_ForAll (a, apply_type_subst (StringMap.remove a subst) b);;


let apply_subst_to_env subst type_env =
  StringMap.map (apply_type_subst subst) type_env;;


exception EXC of string;;

let rec free_vars hm_lambda =
  match hm_lambda with
  | HM_Var a -> StringSet.singleton a
  | HM_App (a, b) -> StringSet.union (free_vars a) (free_vars b)
  | HM_Abs (a, b) -> StringSet.remove a (free_vars b)
  | HM_Let (a, b, c) ->
    let free_vars_c = StringSet.remove a (free_vars c) in
    StringSet.union (free_vars b) free_vars_c;;

let rec free_types hm_type =
  match hm_type with
  | HM_Elem a  -> StringSet.singleton a
  | HM_Arrow (a, b) -> StringSet.union (free_types a) (free_types b)
  | HM_ForAll (a, b) -> StringSet.remove a (free_types b);;

let algorithm_w hm_lambda = 
  let several_subst subst1 subst2 =
  (let subst2 = StringMap.map (apply_type_subst subst1) subst2 in
  StringMap.merge (fun key v1 v2 ->
      match (v1, v2) with
      | (None, None) -> None
      | (Some v, None) -> Some v
      | (None, Some v) -> Some v
      | (Some v1, Some v2) -> Some v2) subst1 subst2) in
  let rec represent hm_type = (match hm_type with
    | HM_ForAll (a, b) ->
      let subst = StringMap.singleton a (HM_Elem (Stream.next unique_var)) in
      apply_type_subst subst (represent b)
    | hm_type -> hm_type) in
  let new_type = HM_Elem (Stream.next unique_var) in
  let rec algorithm_w_rec type_env hm_lambda =
    match hm_lambda with
    | HM_Var a when StringMap.mem a type_env -> 
      (StringMap.empty, match (StringMap.find a type_env) with
        | HM_ForAll (a, b) ->
          let subst = StringMap.singleton a (HM_Elem (Stream.next unique_var)) in
          apply_type_subst subst (represent b)
        | hm_type -> hm_type
    )
    | HM_Var a -> raise (EXC "Free var!")
    | HM_App (a, b) ->
      (let (s1, t1) = algorithm_w_rec type_env a in
       let (s2, t2) = algorithm_w_rec (apply_subst_to_env s1 type_env) b in
       let equation = (term_of_hm_type (apply_type_subst s2 t1), term_of_hm_type (HM_Arrow (t2, new_type))) in
       match solve_system [equation] with
       | None -> raise (EXC "Couldn't solve")
       | Some answer ->
         let add_subst (str, term) = StringMap.add str (hm_type_of_term term) in
         let v = List.fold_right add_subst answer StringMap.empty in
         let unifier = several_subst v (several_subst s2 s1) in
         (unifier, apply_type_subst unifier new_type))
    | HM_Abs (a, b) ->
      let type_env = StringMap.add a new_type (StringMap.remove a type_env) in
      let (s1, t1) = algorithm_w_rec type_env b in
      (s1, HM_Arrow (apply_type_subst s1 new_type, t1))
    | HM_Let (a, b, c) ->
      let gen type_env hm_type = (
      let add_free_types key value = StringSet.union (free_types value) in
      let free_env_types = StringMap.fold add_free_types type_env StringSet.empty in
      let free_hm_types = free_types hm_type in
      let new_forall_vars = StringSet.diff free_hm_types free_env_types in
      let add_quantifier var hm_type = HM_ForAll (var, hm_type) in
      StringSet.fold add_quantifier new_forall_vars hm_type) in
      let (s1, t1) = algorithm_w_rec type_env b in
      let a_type = gen (apply_subst_to_env s1 type_env) t1 in
      let type_env = apply_subst_to_env s1 (StringMap.remove a type_env) in
      let type_env = StringMap.add a a_type type_env in
      let (s2, t2) = algorithm_w_rec type_env c in
      (several_subst s2 s1, t2)
  in
  let bound_to_unique v = StringMap.add v (HM_Elem (Stream.next unique_var)) in
  let type_environment = StringSet.fold bound_to_unique (free_vars hm_lambda) StringMap.empty in
  try
    let (unifier, hm_type) = algorithm_w_rec type_environment hm_lambda in
    Some (StringMap.bindings unifier, hm_type)
  with (EXC e) -> None;;

let rec list_to_string x str = match x with
        |[]-> str
        |h::t -> type_to_string h (list_to_string t str)

and type_to_string x str = match x with 
        |Var a -> a ^ str
        |Fun(a, b) -> "(" ^ a ^ (list_to_string b str) ^ ")" ;;

let test t = 
  let ans1 = algorithm_w t in
  match ans1 with 
  | Some (l, s) -> print_string ((type_to_string (term_of_hm_type s) "") ^ "\n")
  | None -> print_string "";;


let simpleTest = HM_Abs("x", HM_Var("x"));;
let secondTest = HM_Let("w", HM_Abs("f", HM_Abs("x", HM_App(HM_Var("f"), HM_App(HM_Var("f"), HM_Var("x"))))), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_Var("w"))))))))))))));; 

test simpleTest;;
(* test secondTest;; *)

