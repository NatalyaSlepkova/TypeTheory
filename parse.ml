open Opal
open Printf

type algebraic_term = Var of string | Fun of string * (algebraic_term list)

let rec list_to_string x str = match x with
        |[]-> str
        |h::t -> type_to_string h (" " ^  (list_to_string t str))
and type_to_string x str = match x with 
        |Var a -> a ^ " " ^ str
        |Fun(a, b) ->  a ^ " (" ^ (list_to_string b str) ^ ")" ;;

let letter = range 'a' 'z'
let digit = range '0' '9'
let identifier = letter <~> many alpha_num

(*let first cl = match cl with
	h::t -> h

let () =
  let input = LazyStream.of_channel stdin in
  match parse identifier input with
  | Some ans -> print_char (first ans)
  | None -> print_endline "ERROR!" *)
