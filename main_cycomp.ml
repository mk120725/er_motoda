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
open PlNormalization
open PlLabelElimination
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
  (* def of ls *)
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
  
  print_string "------input------\n";
  NewSyntax.Entl.println entl;

  let start = Sys.time() in
  (* Nomalization *)

  let ant = normalization entl.ant in
  let suc = normalization entl.suc in
  let nfentl = NewSyntax.Entl.create [] ant suc in
  let nfentls = [nfentl] in

  
  print_string "------normalizaiton------\n";
  NewSyntax.Entl.println nfentl;
  
  let fvs = Tools.unionLst (NewSyntax.SH.fv ant) (NewSyntax.SH.fv suc) in
  let tfvs = NewSyntax.SHterm.Nil::(List.map (fun x -> NewSyntax.SHterm.Var x) fvs) in
  let tpairs = Tools.makeCombPairs tfvs in
  
  let rec print_entls ts =
    match ts with
    | [] -> ()
    | t::rest -> NewSyntax.Entl.println t;
                 print_entls rest
  in

  let print_entls_op ts_op =
    match ts_op with
    | None -> print_string "None\n"
    | Some ts -> print_entls ts
  in

  let rec print_entls_cc ts =
    match ts with
    | [] -> ()
    | t::rest -> CcSyntax.Entl.println t;
                 print_entls_cc rest
  in

  let print_entls_cc_op ts_op =
    match ts_op with
    | None -> print_string "None\n"
    | Some ts -> print_entls_cc ts
  in

  (* Case analysis *)
  let newsatcheck (ent : NewSyntax.Entl.t) =
    let ccent = New2cc.new2cc_entl ent in
    CcSatcheck.decideSatMain(ccent,ls_def)
  in

  let rec case_each (entls : NewSyntax.Entl.t list) (t1,t2) : NewSyntax.Entl.t list =
    match entls with 
    | [] -> []
    | t::rest -> 
      let case_eq  = 
        NewSyntax.Entl.add_pureexp t (NewSyntax.SHpureExp.Eq(t1,t2)) in 
      let case_neq = 
        NewSyntax.Entl.add_pureexp t (NewSyntax.SHpureExp.NEq(t1,t2)) in 

      if (newsatcheck case_eq)
      then
        if (newsatcheck case_neq)
        then
          case_eq::case_neq::(case_each rest (t1,t2))
        else
          case_eq::(case_each rest (t1,t2))
      else
        if (newsatcheck case_neq)
        then
          case_neq::(case_each rest (t1,t2))
        else
          case_each rest (t1,t2) 
  in

  let rec case_each_pairs (entls : NewSyntax.Entl.t list) ts : NewSyntax.Entl.t list =
    match ts with
    | [] -> entls
    | p::rest -> case_each_pairs (case_each entls p) rest
  in
  
  let ca_entls = case_each_pairs nfentls tpairs in

  
  print_string "------case analysis------\n";
  print_entls ca_entls;
  

  let ca_entls = List.map NewSyntax.Entl.eq_sub ca_entls in

  
  print_string "------case analysis2------\n";
  print_entls ca_entls;
  
  
  let le_entls = 
    match PlLabelElimination.lab_elims ca_entls with
    | None -> None
    | Some entls -> Some entls
  in

  print_string "------label elimination------\n";
  print_entls_op le_entls;

  let cc_le_entls =
    match le_entls with
      None -> None
    | Some entls -> Some (List.map New2cc.new2cc_entl
                            (List.map NewSyntax.Entl.erase_up
                               (List.map NewSyntax.Entl.erase_pure entls))) in

  print_string "------new2cc------\n";
  print_entls_cc_op cc_le_entls;

  let rec entls_check ts = 
    match ts with 
    | [] -> print_endline "Valid"
    | t::rest ->
       CcSyntax.Entl.println t;
      if (CcEntlcheckControl.ccMain (t,ls_def))
      then
        (
          print_endline "ok";
          entls_check rest
        )
      else 
        print_endline "Invalid"
  in

  let entls_check_op ts_op =
    match ts_op with
    | None -> print_endline "Invalid"
    | Some ts -> entls_check ts
  in

  print_string "------entls_check------\n";
  entls_check_op cc_le_entls;

  let end_ = Sys.time () in

  let time = end_ -. start in

  Printf.printf "time:%f" time;
 
 ;;
 
