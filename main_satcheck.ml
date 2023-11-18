(*------------------------------------------------------*)
(*  Satisfiability Checker			*)
(*  compile:   $ make_satcheck		*)
(*  usage:       $ satcheck < input > output	*)
(*  satcheck checks satisfiability of given sym.hp *)
(*------------------------------------------------------*)
open Tools;;
open CcSyntax;;
open CcParser;;
open CcSatcheck;;
open SlSyntax;;

(* read from file *)
let inputstr_stdin () =
let x = ref "" in
try
while true do
x := !x ^ (input_line stdin) ^ "\n"
done ;
"" (* dummy *)
with End_of_file -> !x ;;

(* parser *)
let parse str = 
  CcParser.main CcLexer.token 
    (Lexing.from_string str)
;;

(* Flags *)
type flag = { mutable debug : bool }
;;
let f = { debug = false; } 
;;
let set_debug () = f.debug <- true
;;
let unset_debug () = f.debug <- false
;;

(* Options *)
let f_help () = print_endline "help help";;
    
let speclist = [
  ("-debug", Arg.Unit set_debug, "Debugging mode");
];;

let usagemsg = "USAGE:\n $ ccsatcheck < FILE.cy\n";;
let f_anon s = print_endline s;;

(* main *)
let () =
  Arg.parse speclist f_anon usagemsg;
  let str = inputstr_stdin () in
  let ps = parse str in
  match decideSatCC_flag f.debug ps with
  | false -> if f.debug then print_endline "\nUnsat" else ()
  | true -> if f.debug then print_endline "\nSat" else ()
;;
