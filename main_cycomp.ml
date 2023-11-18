(*---------------------------------------------------------*)
(*  CYCOMP : entailment checker for CYclic COMPlete system *)
(*  compile: $ make -f makefile_cycomp                     *)
(*  usage:   $ cycomp < input                              *)
(*---------------------------------------------------------*)
open Tools
open Ratio
open CcSyntax
open NewSyntax
open New2cc
open NewParser
(* open PlNormalization *)
open CcSatcheck
open SlSyntax
open CcEntlcheck
open CcEntlcheckControl

module Opt = Options.Cycomp

(* read from file *)
let inputstr_stdin () =
let x = ref "" in
try
while true do
x := !x ^ (input_line stdin) ^ "\n"
done ;
"" (* dummy *)
with End_of_file -> !x ;;

let inputstr_file filename =
  let x = ref "" in
  let ic = open_in filename in
  try
	while true do
	  x := !x ^ (input_line ic) ^ "\n"
	done ;
	"" (* dummy *)
  with End_of_file -> close_in ic;!x
;;

(* parser *)
let parse str = 
  NewParser.main NewLexer.token 
    (Lexing.from_string str)
;;

(* Options *)
let f_help () = print_endline "help help";;

let _fname = ref "";;
let set_filename filename = _fname := filename;;

let msgUsage = "USAGE: cycomp -f FILE.cy\n";;

let speclist = [
    ("-f",Arg.String set_filename,"");
    ("-opt",Arg.Unit Opt.setOpt,"Optimized mode");
    ("-full",Arg.Unit Opt.setFull,"Full mode (do subset-split case analysis)");
    ("-debug", Arg.Unit Opt.setDebug, "Debugging mode");
];;





(* main *)
let () =
  let display_message () = print_endline msgUsage in
  Arg.parse speclist print_endline msgUsage;
  if !_fname = "" then display_message () else
  let str = inputstr_file !_fname in
  let entl = parse str in
  (* 

     1. normalization : newSyntax.SH.t -> newSyntax.SH.t

     let entl_nf : NewSyntax.Entl
     = { up = []; ant = normalize entl.ant; suc = normalize entl.suc }
     
     2. case_analysis : newSyntax.Entl -> newSyntax.Entl list
     
     let entls_inj : NewSyntax.Entl list
     = case_analysis entl_nf
     
     3. label-elim : newSyntax.Entl.t -> newSyntax.Entl.t list
     
     let entls_lf : NewSyntax.Entl list
     = List.Fold_left (@) [] (List.map label_elim entls_inj)

     4. data translation from newSyntax to ccSyntax

     let entls_cc : CcSyntax.Entl list
     = List.map new2cc entls_lf
   *)

  NewSyntax.Entl.println entl;

  let ccentl = New2cc.new2cc_entl entl in
  
  CcSyntax.Entl.println ccentl;
  
  (* inductive def. for ls(x,y) (nonempty)
     ls(x,y) = x->y | ex z(x->z * ls(z,y) *)
  let var = CcSyntax.var in
  let ls_def : CcSyntax.IndSys.t = [
      ("ls",
       ["x"; "y"],
       [CcSyntax.SH.create [] [] [] [] [
            (Pto(var "x", [var "y"]),[])
          ];
        CcSyntax.SH.create ["z"] [] [] [] [
            (Pto(var "x", [var "z"]),[]);
            (Pr("ls",[var "z"; var "y"]),[])
          ]
       ]
      )
    ] in
  CcSyntax.IndSys.println ls_def;
                                 
(*
  let ps = parse str in
  let (e,dd) = ps in
  Opt.sayifDebug "[Entailment]";
  Opt.doifDebug (fun _ -> CcSyntax.Entl.println e) ();
  Opt.sayifDebug "";
  Opt.sayifDebug "[Inductive System]";
  Opt.doifDebug (fun _ -> CcSyntax.IndSys.println dd) ();
  Opt.sayifDebug "====================";
  match CcEntlcheckControl.ccMain ps with
  | true -> print_endline "Valid"
  | false -> print_endline "Invalid"
 *)
;;
