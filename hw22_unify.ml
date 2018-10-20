(*---ocamlc -o HW22 opal.ml hw22_unify.mli hw22_unify.ml---*)

open Opal
open List
open Printf

type algebraic_term = Var of string | Fun of string * (algebraic_term list)

(*----PARSER COMBINATOR----*)

let rec list_to_string x str = match x with
        |[]-> str
        |h::t -> type_to_string h (" " ^  (list_to_string t str))
and type_to_string x str = match x with 
        |Var a -> a ^ " " ^ str
        |Fun(a, b) ->  a ^ " (" ^ (list_to_string b str) ^ ")" ;;

let cl2s cl = String.concat "" (List.map (String.make 1) cl)

let letter = range 'a' 'z'
let identifier = (letter <~> many alpha_num) => cl2s

let lift4 f a b c d = a >>= (fun va -> b >>= (fun vb -> c >>= (fun vc -> d >>= (fun vd -> return (f va vb vc vd)))))

let rec termList input = sep_by term (exactly ',') input
and term input = (functionCall <|> variable) input
and variable input = (identifier => (fun x -> Var x)) input
and functionCall input = (lift4 (fun idf s1 terms s2 -> Fun (idf, terms)) identifier (exactly '(') termList (exactly ')')) input 


let rec concat_func_names_in_algeb_term x = match x with
	Var y -> y
	| Fun (name, ls) -> List.fold_left (fun f s -> f ^ concat_func_names_in_algeb_term s) name ls;;

let rec concat_all_functions_names_in_list x = match x with
	[] -> "end"
	| (fs, sn)::tl -> (concat_func_names_in_algeb_term fs) ^ (concat_func_names_in_algeb_term sn) 
						^ (concat_all_functions_names_in_list tl);;

let system_to_equation x = let func_concats = concat_all_functions_names_in_list x in		
							(Fun (func_concats, List.map fst x), Fun (func_concats, List.map snd x));;
						
(*----APPLY SUBSTITUTION----*)

let rec find_in_list f l = match l with
	[] -> None
	| h::t -> if f h then Some h else find_in_list f t

let rec apply_substitution x y = match y with
	Var v -> (match (find_in_list (fun (f, s) -> f = v) x) with
		(Some (var_name, term)) -> term
		| None -> Var v
		)			
	| (Fun (f, data)) -> Fun (f, (List.map (apply_substitution x) data));;

let apply_substitution_to_sol sub sol = 
	List.map (fun(a, b) -> ((a, apply_substitution sub b))) sol;;
						
let rec closure ls sol = match ls with 
	[] -> sol
	| (var_name, term)::tail -> closure (apply_substitution_to_sol ((var_name, term)::sol) tail) ((var_name, term)::sol);;

(*----CHECK SOLUTION----*)

let rec check_to_lists predicate list1 list2 = (List.length list1) = List.length list2 
		&& List.fold_left2 (fun pr b c -> pr && predicate b c) true list1 list2;;
	
let rec equals t1 t2 = match (t1, t2) with 
	(Var a, Var b) -> a = b
	| (Fun (a, la), Fun (b, lb)) -> a = b && check_to_lists equals la lb
	| _ -> false;;

		
let rec check_terms t1 t2 = match (t1, t2) with
        (Var a, Var b) -> a = b
        | (Fun(f, l1), Fun(g, l2)) -> f = g && check_lists l1 l2 
        | _ -> false

    and check_lists l1 l2 = match (l1, l2) with
        ([], []) -> true
        | (fst1::t1), (fst2::t2) -> (check_terms fst1 fst2) && (check_lists t1 t2) 
        | _ -> false;;

let view_with_subst x e = match e with
    (left, right) -> check_terms (apply_substitution x left) (apply_substitution x right)

let rec check_solution x sys = match sys with
    [] -> true
    | h::t -> (view_with_subst x h) && (check_solution x t);;
		
let rec contains_var var alg = match alg with
	Var v -> var = v
	| Fun (name, ls) -> List.exists (contains_var var) ls;;
	
let rec list_to_string x str = match x with
        |[]-> str
        |h::t -> type_to_string h (list_to_string t str)

and type_to_string x str = match x with 
        |Var a -> a ^ str
        |Fun(a, b) -> "(" ^ a ^ (list_to_string b str) ^ ")" ;;

let solution_to_string sol = 
		String.concat "\n" (List.map (fun (var_name, term) -> (var_name ^ "=" ^ (type_to_string term ""))) sol);;

(*----SOLVE SYSTEM----*)

exception EXC of string;;

let rec solve ls prefix = match ls with
	[] -> closure (prefix) [] 
	| (Fun (ln, ll), Fun (rn, rl))::tail -> 
		if ln = rn then 
			solve (List.append (List.map2 (fun a b -> (a, b)) ll rl) tail) prefix
		else (
			raise (EXC "")
		)
	| (Fun (n, l), Var v)::tail -> solve ((Var v, Fun (n, l))::tail) prefix 
	| (Var var, r)::tail -> 
		if equals (Var var) r then (
			solve tail prefix 
		) else if contains_var var r then (
			raise (EXC "")
		) else (
			solve (List.map (fun(a, b) -> (apply_substitution [var, r] a,
					apply_substitution [var, r] b)) tail)
				((var, r)::prefix)
		);;

let solve_system sys = 
	try 
		let result = solve sys [] in
			print_string ((solution_to_string result) ^ "\n");
			(Some result)
	with (EXC what) -> print_string (what ^ "\n");
None;;

let left = "f1(k,f2(l,f3(m,f4(n,f5(o)))))"
let right = "f1(f2(f3(f4(f5(o),n),m),l),k)"
let solution left right = match (parse termList left, parse termList right) with
  | (Some lt, Some rt) -> 
    solve_system (combine lt rt); ()
  | _ -> print_endline "ERROR!";; 

(* let file = "test.txt"
let () =
  let ic = open_in file in
  try 
		let left = LazyStream.of_string (input_line ic) in   
		let right = LazyStream.of_string (input_line ic) in
    solution left right;
    close_in ic
  
  with e ->                   
    close_in_noerr ic;
    raise e *)
                 