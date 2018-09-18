type algebraic_term = Var of string | Fun of string * (algebraic_term list)

(*----SYSTEM TO EQUATION----*)

let rec rename_list l str = match l with
        [] -> str
        | fst::t -> rename_alg_term fst (rename_list t str)     

and rename_alg_term term str = match term with
        Var v -> str ^ v
        | Fun(name, l) -> name ^ (rename_list l str);;

let rec rename_system sys str = match sys with
        [] -> str       
        | (fst,scn)::t -> rename_alg_term fst (rename_alg_term scn (rename_system t str));; 

let get_new_name s = rename_system s "new";;

let rec split sys l r = match sys with 
        [] -> l, r
        |(fst, scn)::t -> split t (fst::l) (scn::r);; 

let system_to_equation sys = 
        let l, r = split sys [] [] in
        let newn = get_new_name sys in (Fun(newn, l), Fun(newn, r));;

(*----APPLY SUBSTITUTION----*)

module MAP = Map.Make(String)

let apply_substitution subst term = 
    let rec init subst map = match subst with 
        [] -> map
        | (fst, scn)::t -> init t (MAP.add fst scn map) in
                            let map = init subst MAP.empty in
                                let rec processing_term term = match term with
                                    Var a -> if MAP.mem a map then 
                                                MAP.find a map 
                                            else Var a
                                    | Fun(fst, l) -> Fun(fst, processing_list l)
                                and processing_list l = 
                                    let rec impl l ans = match l with 
                                            [] -> ans 
                                            | h::t -> impl t ((processing_term h) :: ans) in
                                    impl l [] in
                                        processing_term term;;

(*----CHECK SOLUTION----*)

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

(*----SOLVE SYSTEM----*)

exception EXC of string;;

module SET = Set.Make(String)

let rec list_to_string x str = match x with
        |[]-> str
        |h::t -> type_to_string h (" " ^  (list_to_string t str))

and type_to_string x str = match x with 
        |Var a -> a ^ " " ^ str
        |Fun(a, b) -> "(" ^ a ^ " " ^ (list_to_string b str) ^ ")" ;;

let rec system_to_string x str = match x with
        |[] -> str
        |(l,r)::t -> type_to_string l "" ^ " = " ^  type_to_string r "" ^ "\n" ^ system_to_string t str;;

let print_system x = print_string (system_to_string x "\n");;

let rec list_contains x str = match x with
         | [] -> false
         | fst::t -> type_contains fst str || list_contains t str

and type_contains x str = match x with
    | Var a -> a = str
    | Fun(t, l) -> list_contains l str;;

let rec system_subst expr x sol = match x with
    | [] -> List.rev sol
    | (fst, scn)::t -> 
        system_subst expr t (((apply_substitution expr fst), (apply_substitution expr scn))::sol);; 

let solution_step sys fst scn set = 
    let rec get_solution l r sol = match (l, r) with
        ([], c) -> sol
        | (lh::lt, rh::rt) -> get_solution lt rt ((lh, rh)::sol) in 
    match (fst, scn) with
    (expr, Var x) -> ((List.append sys [Var x, expr]), set)
    | (Var x, expr) -> if (type_contains expr x) then raise(EXC("The system isn't compatible. nafig tt"))
                        else let set = SET.add x set in
                            ((List.append(system_subst [x, expr] sys []) [fst, scn]), set) 
    | (Fun(a, b), Fun(c, d)) -> if (a <> c || List.length d <> List.length b) 
                                    then raise(EXC("The system isn't compatible nafig tt"))
                                else (List.append sys (get_solution b d []), set);;

let rec var_reduction sys sol = match sys with
    [] -> List.rev sol
    | (Var x, expr)::t -> var_reduction t ((x, expr)::sol);;

let rec solving x set  = match x with
    [] -> x
    | (fst, scn)::t -> if (type_to_string fst "" = type_to_string scn "")
                    then solving t set
                  else let new_list, new_set = (solution_step t fst scn set) in
                    if ((List.length new_list) = SET.cardinal new_set) then new_list
                    else solving new_list new_set;;

let solve_system sys =
    try
        let result = solving sys SET.empty in print_string (system_to_string result "");
                (Some (var_reduction result []))
    with (EXC what) -> (print_string (what ^ "\n"));
None;;

let at4 = Var "x";;
let at8 = Var "x";;
let isys0 = [at4, at8];;

let isys1 = [
              Fun("f",[Var "y"; Fun("h",[Var "x"; Var "y"])]),
              Fun("f",[Fun("g",[Var "a"; Var "b"]); Fun("h", [Var "x"; Var "x"])]);
              Fun("h",[Var "x"; Var "y"]),
              Fun("h", [Var "a"; Var "a"])];;

(* print_system isys1;; *)
solve_system isys0;;

