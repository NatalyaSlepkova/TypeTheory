(*ocamlfind ocamlopt -o Ex -linkpkg \
  -package angstrom,angstrom.unix \
  example2.ml*)
open Angstrom
open Printf

type algebraic_term = Var of string | Fun of string * (algebraic_term list)

let rec list_to_string x str = match x with
        |[]-> str
        |h::t -> type_to_string h (" " ^  (list_to_string t str))

and type_to_string x str = match x with 
        |Var a -> a ^ " " ^ str
        |Fun(a, b) ->  a ^ " (" ^ (list_to_string b str) ^ ")" ;;

let identifier =
    take_while1 (function 'a' .. 'z' -> true | '0' .. '9' -> true | _ -> false)
let variable = identifier >>| (fun x -> Var x)
let termList = fix (fun m -> 
  let functionCall = lift4 (fun idf s1 terms s2 -> Fun (idf, terms)) identifier (char '(') m (char ')') in
  let term = functionCall
  in sep_by (char ',') term)
let eval2 (str:string) =
  match parse_only termList (`String str) with
  | Result.Ok v      -> v
  | Result.Error msg -> failwith msg

let a = eval2 "a8(b5()),c7()";;
print_string (list_to_string a "");; 