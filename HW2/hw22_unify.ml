open Opal
open List

type algebraic_term = Var of string | Fun of string * (algebraic_term list)

(*---Parser Combinator---*)

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
						

let rec apply_substitution x y = match y with
	Var v -> (try let (pf, ps) = List.find (fun (f, s) -> f = v) x in 
				ps with _ -> Var v)
	| Fun (f, data) -> Fun (f, (List.map (fun term -> apply_substitution x term) data));;


exception Key_not_found of string;;

let rec check_to_lists predicate list1 list2 = (List.length list1) = List.length list2 
		&& List.fold_left2 (fun pr b c -> pr && predicate b c) true list1 list2;;
	
let rec equals t1 t2 = match (t1, t2) with 
	(Var a, Var b) -> a = b
	| (Fun (a, la), Fun (b, lb)) -> a = b && check_to_lists equals la lb
	| _ -> false;;

		
let rec check_equation_solution sol fs sn = match (fs, sn) with
	(Fun (nl, lil), Fun(nr, lir)) -> nl = nr && check_to_lists (check_equation_solution sol) lil lir
	| (a, b) -> equals (apply_substitution sol a) (apply_substitution sol b);;
		
let rec check_solution x y = match y with 
	[] -> true 
	| (fs, sn)::tl -> check_equation_solution x fs sn && check_solution x tl;;
	

exception NoSolution;;
	

exception Error;;
		
(* Checks if alg term contains var *)
let rec contains_var var alg = match alg with
	Var v -> var = v
	| Fun (name, ls) -> List.exists (contains_var var) ls;;
	

let rec term_to_string term = match term with 
	Var a -> a
	| Fun(name, ls) -> name ^ "(" ^ (data_to_string ls)^ ")"
and data_to_string data = match data with
	[] -> ""
	| last::[] -> term_to_string last
	| hd::tail -> (term_to_string hd) ^ "," ^ (data_to_string tail);;
let print_term term = print_string (term_to_string term);;
let print_term_e term = print_term term; print_newline();;

let apply_substitution_to_sol sub sol = 
	List.map (fun(a, b) -> ((*print_string a;
						print_string "\n";
							print_term b;
						print_string "\n";
						print_term (apply_substitution sub b);
						print_string "\n";
						print_string "\n";*)
	(a, apply_substitution sub b))) sol;;
						
let rec zamykanie ls sol = match ls with 
	[] -> sol
	| (var, d)::tail -> 
(*		print_string ("var:"^var^"=");
		print_term d;
		print_string "\n";*)
				
	zamykanie (apply_substitution_to_sol ((var, d)::sol) tail)
					((var, d)::sol);;
							

let rec solve ls prefix = match ls with
	[] -> zamykanie (prefix) [] 
	| (Fun (ln, ll), Fun (rn, rl))::tail -> 
		if ln = rn then 
			solve (List.append (List.map2 (fun a b -> (a, b)) ll rl) tail) prefix
		else (
			print_endline ("got different fucntions "^ln^rn);
			raise NoSolution
		)
	| (Fun (n, l), Var v)::tail -> solve ((Var v, Fun (n, l))::tail) prefix 
	| (Var var, r)::tail -> 
		if equals (Var var) r then (
			solve tail prefix 
		) else if contains_var var r then (
			print_endline ("got "^var^" in "^(term_to_string r));
			raise NoSolution
		) else (
			solve (List.map (fun(a, b) -> (apply_substitution [var, r] a,
					apply_substitution [var, r] b)) tail)
				((var, r)::prefix)
		);;

let solve_system equations = try Some (solve equations []) with _ -> None;;

let checker system = 
	List.iter (fun (lhs, rhs) -> print_term(lhs); print_string ("="); print_term rhs; print_string "\n") system;
	print_string "aaa\n";

	match solve_system system with 
	None -> print_string "none\n";
	| Some ls -> 
		List.iter (fun (name, term) -> print_string (name^"="); print_term term; print_string "\n") ls;
		print_string "----------\n";
		if check_solution ls system = false then 
			print_endline "fail"
		else 
			print_endline "correct solution";
	;;

let left = "f1(k,f2(l,f3(m,f4(n,f5(o)))))"
let right = "f1(f2(f3(f4(f5(o),n),m),l),k)"
let solution left right = match (parse termList left, parse termList right) with
  | (Some lt, Some rt) -> checker (combine lt rt) 
  | _ -> print_endline "ERROR!";; 

let left2 = LazyStream.of_string left;;
let right2 = LazyStream.of_string right;;
solution left2 right2;;
