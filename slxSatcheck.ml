(*----------------------------*)
(* Satchecker for Extended SH *)
(*----------------------------*)
open Tools
open SlxSyntax

module SL = SlSyntax

(* order t u = 0 if t = u *)
(* order t u = 1 if t > u *)
(* order t u = -1 if t < u *)
let updateOne order eqPairs kk =
  let n = List.length eqPairs in
  let lookup k =
	let r = ref [] in
	for i = 0 to n-1 do
	  let (x,y) = List.nth eqPairs i in
	  if x = k && order y x = 1
	  then r := [y]
	  else
		if y = k && order x y = 1
		then r := [x]
		else ()
	done;
	!r
  in
  let rec aux res ll =
	match ll with
	| [] -> List.rev res
	| x::xx ->
	   match lookup x with
	   | [] -> aux (x::res) xx
	   | y::_ -> aux (y::res) xx
  in
  aux [] kk
;;

let update order eqPairs kk =
  let res = ref [] in
  let res2 = ref kk in
  while !res <> !res2 do
	res := !res2;
	res2 := updateOne order eqPairs !res;
  done;
  !res
;;

let areDisjointUpto pp vv1 vv2 =
  let module T = SHterm in
  let module L = List in
  let module P = SHpureExp in
  let module PP = SHpure in
  let tt1 = L.map (fun v -> T.Var v) vv1 in
  let tt2 = L.map (fun v -> T.Var v) vv2 in
  let (ppEq,_) = PP.split pp in
  let tpairs = L.flatten
	(L.map (function P.Eq(t,u) -> [(t,u)] | _ -> []) ppEq) 
  in
  let nf1 = update T.order tpairs tt1 in
  let nf2 = update T.order tpairs tt2 in
  List.for_all (fun x -> not(List.mem x nf2)) nf1
;;

(* Satchecking for the extended SH *)
let satcheck yy (shx : SH.t) =
  let (vv,pp,ss) = shx in
  let checkBPT (vv0,pp0,_) = areDisjointUpto pp0 vv0 vv in
  SlSatcheck.satcheckCore checkBPT yy (pp,ss)  

(* Making model for Satisfiable Extended SH   *)
(* It returns *Extended* SH if a model exists *)
let createModel isx shx tr =
  let is = IS.forget isx in
  let (vv,pp,ss) = shx in
  match SlSatcheck.createModel is (pp,ss) tr with
  | None -> None
  | Some(pp1,ss1) -> Some(vv,pp1,ss1)

(*--------------------------------------*)
(* Deciding Extended SH                 *)
(*--------------------------------------*)
let decideSat isx shx =
  let is = IS.forget isx in
  let (record,yy) = SlSatcheck.computeBasePair is in
  match satcheck yy shx, Options.Satchecker.modelFlag () with
  | None,_ -> (None,record)
  | Some(vv,pp,tr),true ->
	 let model = createModel isx shx tr in
	 (Some(vv,pp,model),record)
  | Some(vv,pp,tr),false -> (Some(vv,pp,None),record)
;;
