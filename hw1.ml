open Printf

(*----Arithmetic operations----*)

type peano = Z | S of peano;;

let inc a = S a;;

let rec add a b = match a with
	Z -> b
	| S c -> inc(add c b);; 

let rec mul a b = match b with
	Z -> Z
	| S d -> add (mul a d) a;;

let rec sub a b = match (a, b) with
	(Z, d) -> Z
	| (c, Z) -> c
	| (S c, S d) -> sub c d;;

let rec power a b = match b with
	Z -> inc Z
	| S d -> mul (power a d) a;;

let rec peano_of_int a = match a with
	0 -> Z
	| c -> inc(peano_of_int(c - 1));;

let rec int_of_peano p = match p with
	Z -> 0
	| S x -> 1 + int_of_peano x;;

(*----MergeSort and Reverse----*)

let rec rev_add old now = match old with
	[] -> now
	| b::e -> rev_add e (b::now);;

let rev l = rev_add l [];;

(*----*)

let rec split_add l f s = match l with
	[] -> (f, s)
	| b::e -> split_add e s (b::f);;

let rec split l = split_add l [] [];;

let rec merge a b = match a with
	[] -> b
	| a1::a2 -> match b with
		[] -> a
		| b1::b2 -> if ((<) b1 a1) then (b1::(merge a b2)) 
				else (a1::(merge a2 b));;

let rec merge_sort a = match a with
	[] -> []
    | single::[] -> single::[]
	| left::right -> let (f, s) = split a in 
	(merge  (merge_sort f) (merge_sort s));;

(*----Parsers----*)

type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec string_of_lambda x = match x with
    Var v -> v 
    | Abs (v, y) -> "" ^ "\\" ^ v ^ "." ^ (string_of_lambda y) ^ ""
    | App (l, r) -> "(" ^ (string_of_lambda l) ^ ") (" ^ (string_of_lambda r) ^ ")";;

let beg_of_string x ind = String.trim (String.sub x 0 ind);;
let en x ind = String.trim (String.sub x ind ((String.length x) - ind));;

let rec find_pos x bal pos = match x.[pos] with 
    ' ' -> if bal = 0 then pos else find_pos x bal (pos - 1)
    | ')' -> find_pos x (bal + 1) (pos - 1)
    | '(' -> if bal = 1 then pos else find_pos x (bal - 1) (pos - 1)
    | _ -> find_pos x bal (pos - 1);;

let rec get_lambda_pos x pos bal = match (bal, x.[pos]) with
    (0, '\\') -> pos - 1
    | (_, '(') -> get_lambda_pos x (pos + 1) (bal - 1)
    | (_, ')') -> get_lambda_pos x (pos + 1) (bal + 1)
    | (_, _) -> get_lambda_pos x (pos + 1) bal;;
    
let rec parse_application x = let pos = find_pos x 0 ((String.length x) - 1) in 
                                if pos = 0 then lambda_of_string_helper (String.sub x 1 ((String.length x) - 2))
                                else 
            try let lam_pos = get_lambda_pos x 0 0 in
                
                App (lambda_of_string_helper (beg_of_string x lam_pos), 
                        lambda_of_string_helper (en x lam_pos))
            with _ -> 
                                App (lambda_of_string_helper (beg_of_string x pos),
                                    lambda_of_string_helper (en x pos))

    
and lambda_of_string_helper x = match (String.get x 0) with
    '\\' -> let ind = String.index x '.' in     
            Abs (String.trim (String.sub x 1 (ind - 1)), lambda_of_string_helper (en x (ind + 1)))
    | _ -> if (String.contains x '(') || (String.contains x ' ') then   
                parse_application x
            else 
                Var x;;

let lambda_of_string x = lambda_of_string_helper (String.trim x);;
                
let print_lambda x = print_string ((string_of_lambda x)^"\n");;