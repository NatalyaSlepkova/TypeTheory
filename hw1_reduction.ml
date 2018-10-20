open Hw1;;

module SET = Set.Make (String);;


(*----FREE_VARS----*)

let rec get_free_vals l busy = match l with
	Var v -> if SET.mem v busy then SET.of_list([]) else SET.of_list(v::[])
	| Abs(v, lambda) -> get_free_vals lambda (SET.add v busy) 
	| App(left, right) -> SET.union (get_free_vals left busy) (get_free_vals right busy);;

let free_vars l = SET.elements (get_free_vals l SET.empty);;


(*----FREE_TO_SUBST----*)

let rec get_free_to_subst blocks l vars = match l with
	Var v -> true
	| Abs(v, lambda) -> if (SET.mem v blocks) then false 
							else if v = vars then true
							else get_free_to_subst blocks lambda vars
	| App(left, right) ->  (get_free_to_subst blocks left vars) && (get_free_to_subst blocks right vars);;

let free_to_subst x l v = 
	let blocks = get_free_vals x SET.empty in 
	get_free_to_subst blocks l v;;


(*----IS NORMAL FORM----*)

let rec is_normal_form l = match l with
	Var v -> true
	| App(Abs(v, lambda), right) -> false
	| Abs(v, lambda) -> is_normal_form lambda
	| App(left, right) -> (is_normal_form left) && (is_normal_form right);;


(*----IS_ALPHA_EQUIVALENT----*)

let rec spell l oldvar newvar = match l with
	Var v -> if v <> oldvar then l 
				else newvar
	| Abs(v, lambda) -> if v = oldvar then l 
							else Abs(v, spell lambda oldvar newvar)
	| App(left, right) -> App(spell left oldvar newvar, spell right oldvar newvar);;


let rec is_alpha_equivalent l1 l2 =
	let k = ref 0 in
	match (l1, l2) with 
		(Var v1, Var v2) -> v1 = v2 
		| (Abs(v1, lambda1), Abs(v2, lambda2)) -> let k2 = !k + 1 in 
											is_alpha_equivalent (spell lambda1 v1 (Var ("x"^(string_of_int k2))))
																(spell lambda2 v2 (Var ("x"^(string_of_int k2))))
		| (App(l1, r1), App(l2, r2)) -> (is_alpha_equivalent l1 l2) && (is_alpha_equivalent r1 r2)
		| _ -> false;;


let l1 = lambda_of_string "\\a.a b";;
let l2 = lambda_of_string "\\b.b a";;
let print_bool a = print_endline (if a then "T" else "F");;
(* print_bool (is_alpha_equivalent l1 l2);;  *)


(*----NORMAL BETA REDUCTION----*)

let rec beta_reduction_step l = match l with
	Var v -> l
	| App(Abs(v, lambda), right) -> spell lambda v right
	| App(left, right) -> if is_normal_form left then App(left, beta_reduction_step right)
							else App(beta_reduction_step left, right)
	| Abs(v, lambda) -> Abs(v, beta_reduction_step lambda);;

module MAP = Map.Make(String)

let k = ref 0;;
let get_newvar() = 
	k := !k + 1;
	("x" ^ string_of_int !k);;

let rec fresh_args l newname = match l with
	Var v -> if MAP.mem v newname then Var (MAP.find v newname) else l
	| Abs(v, lambda) -> let name = get_newvar() in 
						Abs(name, fresh_args lambda (MAP.add v name newname))
	| App(left, right) -> App(fresh_args left newname, fresh_args right newname);;

let normal_beta_reduction l = beta_reduction_step (fresh_args l MAP.empty);;


(*----REDUCE TO NORMAL FORM----*)

type advanced_lambda = AdvVar of string 
					| AdvAbs of (string * advanced_lambda ref) 
					| AdvApp of (advanced_lambda ref * advanced_lambda ref);;

let rec come_down l = match !l with 
	AdvVar v -> Var v
	| AdvAbs(v, lambda) -> Abs(v, come_down lambda)
	| AdvApp(left, right) -> App(come_down left, come_down right)

let rec advance l = match l with
	Var v -> ref (AdvVar v)
	| Abs(v, lambda) -> ref (AdvAbs(v, advance lambda))
	| App(left, right) -> ref (AdvApp(advance left, advance right));;

let rec spell2 l oldvar newvar = match !l with
	AdvVar v -> if v = oldvar then l := !newvar
	| AdvAbs(v, lambda) -> if v <> oldvar then spell2 lambda oldvar newvar
	| AdvApp(left, right) -> spell2 left oldvar newvar;
							spell2 right oldvar newvar;;

let rec renaming l vars = match !l with 
	AdvVar v -> (* print_endline("11"); *) (try ref (MAP.find v vars) with _ -> ref (AdvVar v))
	| AdvAbs(v, lambda) -> (* print_endline("22"); *) let name = get_newvar() in 
							ref (AdvAbs(name, renaming lambda (MAP.add v (AdvVar name) vars))) 
	| AdvApp(left, right) -> (* print_endline("33"); *) ref (AdvApp(renaming left vars, renaming right vars));;

let rec copy_renaming l = renaming l MAP.empty;;

let rec get_reduce_to_normal l = match !l with
	AdvVar _ -> (* print_endline("1"); *) false
	| AdvAbs(v, lambda) -> (* print_endline("2"); *) get_reduce_to_normal lambda
	| AdvApp(left, right) -> match !left with
							| AdvAbs(v, lambda) -> (* print_endline("3a"); *) 
							l := !(copy_renaming lambda);
							spell2 l v right; true
							| _ -> (* print_endline("3b"); *) get_reduce_to_normal left || get_reduce_to_normal right;;

let reduce_to_normal_form l = 
	let adv_l = copy_renaming (advance l) in
		while get_reduce_to_normal adv_l do () done;
		come_down adv_l;; 

(* let l3 = lambda_of_string "(\\x. (x x)) ((\\x. y) z)";;
print_endline("(\\x. (x x)) ((\\x. y) z)");;
print_endline(string_of_lambda (reduce_to_normal_form l3));;
let l4 = lambda_of_string "(\\x. x x x) (\\x. x)";;
print_endline("(\\x. x x x) (\\x. x)");;
print_endline(string_of_lambda (reduce_to_normal_form l4));; *)
