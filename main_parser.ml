(*------------------------------------------------------*)
(*  Parser for CyComp	(Cyclic Complete)				*)
(*  compile:   $ make -f makefile_parser $*				*)
(*  usage:     $ ccparser < input                       *)
(*------------------------------------------------------*)
open Tools;;
open CcSyntax;;
open CcParser;;

let inputstr_stdin () =
  let x = ref "" in
  try
    while true do
      x := !x ^ (input_line stdin) ^ "\n"
    done ;
    "" (* dummy *)
  with End_of_file -> !x
;;

let parse str = 
  CcParser.main CcLexer.token 
    (Lexing.from_string str)
;;

let () =
  let str = inputstr_stdin () in
  let ps = parse str in
  ProgSys.println ps
;;
