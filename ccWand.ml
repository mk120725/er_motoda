open CcSyntax
open Tools

(*------------------------------------------------------------------*)
(* Dependencies between Inductive Predicates                        *)
(*------------------------------------------------------------------*)
let depID (d : IndDef.t) =
  (* dep (pr,_,defL) gathers the ind.preds on which pr depends *)
  let (_,_,defL) = d in
  let ss = L.flatten (L.map (fun h -> L.map fst (h.SH.gm)) defL) in
  let dp s =
	match s with
	| S.Emp -> []
	| S.Pto(_,_) -> []
	| S.Pr(p,_) -> [p]
  in
  dropRed (L.sort compare (L.flatten (L.map dp ss)))

let rec getDirectDep (dd : IndSys.t) a =
  match dd with
  | [] -> []
  | ((a1,_,_) as d) :: dd1 when a = a1 -> depID d
  | _ :: dd1 -> getDirectDep dd1 a

let getDirectDepL dd aa =
  let aa1 = L.flatten (L.map (getDirectDep dd) aa) in
  aa1

let createDepInfo (dd : IndSys.t) =
  (* It gathers ind.pred.names on which the ind.pred. "r" depends *)
  let updateDepInfo info =
	L.map
	  (fun (a,aa) -> (a,dropRed (L.sort compare (aa @ getDirectDepL dd aa))))
	  info
  in
  let _depInfo = ref (L.map (fun (a,_,_)->(a,getDirectDep dd a)) dd) in
  let _depInfoNext = ref (updateDepInfo !_depInfo) in
  while !_depInfo <> !_depInfoNext do
	_depInfo := !_depInfoNext;
	_depInfoNext := updateDepInfo !_depInfo
  done;
  !_depInfo
;;	
	
let dep ddOrig a =
  let fixptDep = createDepInfo ddOrig in
  L.assoc a fixptDep
;;

let depends dd a1 a2 =
  (* a1 depends on a2 under dd *)
  L.mem a2 (dep dd a1)


	
(*-------------------------------------------------------*)
(* Inductive Wands                                       *)
(*-------------------------------------------------------*)
module WandHeaderExp = struct
  (* (Q,t) means Q@1 *)
  (* q is used *)
  type t = string * SHterm.t list

  let to_string ((p,tt) : t) = p ^ "@" ^ (string_of_int (L.length tt))

  let from_string s : t =
	let mkDummy n = L.map (fun _ -> var "?????") (genLst n) in
	match splitString "@" s with
	| nS::p::_ ->
	   let tt = mkDummy (int_of_string nS) in
	   (p,tt)
	| _ -> failwith ""
	   
end
	  
module WandHeader = struct
  (* [(Q,t);(P,tu)] means Q@1:P@2 *)
  (* t and u are terms *)
  (* qq is used *)
  type t = WandHeaderExp.t list

  let rec to_string (qq : t) =
	match qq with
	| [] -> ""
	| q::[]  -> WandHeaderExp.to_string q
	| q::qq1 -> (WandHeaderExp.to_string q) ^ ":" ^ (to_string qq1)

  let from_string s : t =
	let qqStr = splitString ":" s in
	L.map WandHeaderExp.from_string qqStr
	   
end

module WandPred = struct
  (* ([(Q,1);(P,2)],R) means Q@1:P@2-*R *)
  (* w is used *)	
  type t = WandHeader.t * string

  let to_string (w : t) =
	let (qq,p) = w in
	match qq with
	| [] -> p
	| _  -> (WandHeader.to_string qq) ^ "-*" ^ p

  let conv_from_string p tt : t * T.t list =
	(* conv_from_string "Q@1:R@2-*P" returns ([(Q,1);(R,2)],P) *)
	let prP_hdL = splitString "-*" p in
	let prP = L.hd prP_hdL in
	let hdL = L.tl prP_hdL in
	match hdL with
	| [] -> let w = ([], prP) in (w,tt)
	| hdStr::_  ->
	   let qq = WandHeader.from_string hdStr in (* [ (R,["";""]);(Q,[""]) ] *)
	   let argL = L.rev (L.map (fun q -> L.length (snd q)) qq) in (* [1;2] *)
	   let n = (L.length tt) - (L.fold_right ( + ) argL 0) in
	   let ttL = splitListByLength (n::argL) tt in
	   let prL = prP :: (L.rev (L.map fst qq)) in
	   let pr_argL = zipLst prL ttL in
	   let (_,uu),qqTl = L.hd pr_argL, L.tl pr_argL in
       (*
	   let w = (L.rev qqTl,prP) in
        *)
       let w = (qqTl,prP) in
	   (w,uu)

  let from_string p tt : t = fst (conv_from_string p tt)

end

module WandS = struct
  (* Single Wand Spatial expressions *)
  (* 's' is used for them			 *)
  type t =
	| Emp
	| Pto of T.t * T.t list
	| Pr of WandPred.t * T.t list

  let down s =
	match s with
	| Emp -> S.Emp
	| Pto(t,tt) -> S.Pto(t,tt)
	| Pr(w,tt) ->
	   let p = WandPred.to_string w in
	   let qq = fst w in
	   let uu = L.flatten (L.rev (L.map snd qq)) in
	   S.Pr(p,tt@uu)

  let up (s : SHspatExp.t) =
	match s with
	| S.Emp -> Emp
	| S.Pto(t,tt) -> Pto(t,tt)
	| S.Pr(p,tt) ->
	   let (w,uu) = WandPred.conv_from_string p tt in
	   Pr(w,uu)

end

module WS = WandS
  
module WandG = struct
  (* g is used *)
  type t = WandS.t * SHterm.t list

  let down (g : t) : G.t =
	let (s,tt) = g in
	(WandS.down s,tt)

  let up (g : G.t) : t =
	let (s,tt) = g in
	(WandS.up s,tt)
	  
end
  
module WandGG = struct
  (* gg is used *)
  type t = WandG.t list

  let down (gg : t) : GG.t =
	L.map WandG.down gg
	
  let up (gg : GG.t) : t =
	L.map WandG.up gg

end

module WandSH = struct
  (* h is used *)
  type t =
	{
	  mutable ex : string list;
	  mutable up : T.t list;
	  mutable out : T.t list;
	  mutable pi : PP.t;
	  mutable gm : WandGG.t
	}

  let falseSH x n =
	let nilN = L.map (fun _ -> T.Nil) (genLst n) in
	let gg = [ (var x -.> nilN,[]) ] in
	let gg1 = WandGG.up gg in
	{ ex=[]; up=[]; out=[]; pi=[T.Nil <.> T.Nil]; gm=gg1 }
	
  let up (h : SH.t) : t =
	{
	  ex=h.SH.ex;
	  up=h.SH.up;
	  out=h.SH.out;
	  pi=h.SH.pi;
	  gm = WandGG.up h.SH.gm
	}

  let down (h : t) : SH.t =
	{
	  SH.ex=h.ex;
	  SH.up=h.up;
	  SH.out=h.out;
	  SH.pi=h.pi;
	  SH.gm = WandGG.down h.gm
	}	

  let subst (sub : Subst.t) h = up (SH.subst sub (down h))

  let to_string h = SH.to_string (down h)
	
  let println h = SH.println (down h)

end

module WSH = WandSH

module WandIndDef = struct
	
  type t = WandHeader.t * string * string list * WSH.t list

  let up (d : IndDef.t) : t =
	let (p,prm,hh) = d in
	let hh1 = L.map WSH.up hh in
	([],p,prm,hh1)

  let down (d : t) : IndDef.t =
	let (qq,p,prm,hh) = d in
	let p1 = WandPred.to_string (qq,p) in
	let prmL = L.map (fun q -> L.map T.extract (snd q)) qq in
	let prm2 = L.flatten (L.rev prmL) in
	let prm3 = prm @ prm2 in	
	let hh1 = L.map WSH.down hh in
	(p1,prm3,hh1)

  let to_string (d : t) = IndDef.to_string (down d)

  let println d = IndDef.println (down d)

  let normParam (d : t) : t =
	let (qq,p,prm,hh) = d in
	let len = L.length prm in
	let prm1 = L.map (fun i -> "%"^(string_of_int i)) (L.rev (genLst len)) in
	let tt = L.map var prm1 in
	let sub = zipLst prm tt in
	let hh1 = L.map (WSH.subst sub) hh in
	let qq1 = L.rev qq in
	let _qq = ref [] in
	let _c = ref len in
	for j = 0 to L.length qq1 - 1 do
	  let (pj,ttj) = L.nth qq1 j in
	  let nj = L.length ttj in
	  let prmj = L.map (fun i -> "%"^(string_of_int i)) (L.rev (genLstFrom !_c nj)) in
	  let uuj = L.map var prmj in
	  _c := !_c + nj;
	  _qq := (pj,uuj) :: !_qq
	done;
	(!_qq,p,prm1,hh1)
	
end

module WID = WandIndDef

module WandIS = struct

  type t = WID.t list

  let up (dd : IS.t) : t = L.map WID.up dd

  let down (dd : t) : IS.t = L.map WID.down dd

  let to_string (dd : t) = IS.to_string (down dd)

  let println dd = IS.println (down dd)

  let create dwand ddOrig : t =
  (*--------------------------------------------------------------
    create 2 [ (P,x,def1); (Q,xy,def2) ] makes the list of     
    ([],P,x,def1); ([P1],P,x,def1); ([Q2],P,x,def1)
    ([P1;P1],P,x,def1); ([Q2;P1],P,x,def1); ([Q2;Q2],P,x,def1)
    ([],Q,xy,def2); ([Q2],Q,xy,def2); ([Q2;Q2],Q,xy,def2)
     where P < Q and P depends on P,Q and Q depends on Q
     Note that the definition bodies are not updated!!!
  ---------------------------------------------------------------*)
	let ( <=: ) q1 q2 = fst q1 <= fst q2 in
	let predInfo = L.map (fun (p,prm,_) -> (p,L.map var prm)) ddOrig in
	let dd1 = up ddOrig in
	let _prev = ref dd1 in
	let _ans = ref !_prev in
	for i = 0 to dwand - 1 do
	  let _cur = ref [] in
	  for j = 0 to L.length !_prev - 1 do
		let (qq,p,prm,hh) = L.nth !_prev j in
		let q0 = if qq = [] then ("",[]) else L.hd qq in
		for k = 0 to L.length predInfo - 1 do
		  let q = L.nth predInfo k in
		  if q0 <=: q && depends ddOrig p (fst q)
		  then _cur := (q::qq,p,prm,hh) :: !_cur
		  else ()
		done;
	  done;
	  _ans := (L.rev !_cur) @ !_ans;
	  _prev := !_cur
	done;
	L.map WID.normParam (L.rev !_ans)
	
end

module WIS = WandIS
;;

module WandAttr = struct
  (* a is used *)
  type t =
	| Cn of WandHeaderExp.t	     (* Canceller *)
	| Hd of WandHeader.t         (* Additional Header *)
    (* Hd [(P,tu);(Q,t);(R,r)] *)

  let extractHeader a =
	match a with
	| Hd qq -> Some qq
	| _ -> None

end

module WA = WandAttr
;;

let updateWSH_attrs (h : WSH.t) attrL : WSH.t =
  let rec aux vv out up pp ggres gg sub attrL1 = 
	match attrL1,gg with
	| [],[] ->
	   let gm1 = L.rev ggres in
	   let h1 = {WSH.ex=vv; WSH.out=out; WSH.up=up; WSH.pi=pp; WSH.gm=gm1} in
	   WSH.subst sub h1
	| (WA.Cn (p,tt))::attrL2, (WS.Pr(w1,uu1),_)::gg1 when p = (WandPred.to_string w1) ->
	   let z = T.extract (L.hd uu1) in
	   let uu2 = L.tl uu1 in
	   let t2 = L.hd tt in
	   let tt2 = L.tl tt in
	   let sub1 = (z,t2)::sub in
	   let pp1 = (tt2 =..= uu2) @ pp in
	   let vv1 = setminus vv [z] in
	   aux vv1 out up pp1 ggres gg1 sub1 attrL2
	| (WA.Hd qq)::attrL2, (WS.Pr(w1,uu1),xx)::gg1 ->
	   let (qq2,p2) = w1 in
	   (* let uu3 = uu1 @ (L.flatten (L.map snd qq)) in *)
	   let qq3 = L.rev qq in
	   let w3 = (qq3@qq2,p2) in
	   let g3 = (WS.Pr(w3,uu1),xx) in
	   aux vv out up pp (g3::ggres) gg1 sub attrL2
	| _,((WS.Emp,_) as g)::gg1 -> aux vv out up pp (g::ggres) gg1 sub attrL1
	| _,((WS.Pto(_,_),_) as g)::gg1 -> aux vv out up pp (g::ggres) gg1 sub attrL1
	| _,_ -> failwith ""
  in
  aux h.WSH.ex h.WSH.out h.WSH.up h.WSH.pi [] h.WSH.gm [] attrL
;;		   

let mkAllAttrsGG ddOrig (qq : WandHeader.t) (gg : WandGG.t) : WA.t list list =
  let module WA = WandAttr in
  let module WS = WandS in
  let _stack = ref [(qq, Array.make (L.length gg) (WA.Hd []))] in
  let _res = ref [] in
  while !_stack <> [] do
	match L.hd !_stack with
	| ([],arr) ->
	   begin
		 _stack := L.tl !_stack;
		 _res := (Array.to_list arr) :: !_res
	   end
	| ((p,tt)::qq1,arr) ->
	   begin
		 _stack := L.tl !_stack;
		 for j = 0 to L.length gg - 1 do
		   match L.nth gg j with
		   | (WS.Pr(w1,_),_) ->
			  begin
				match arr.(j) with
				| WA.Hd [] when w1 = ([],p) ->
				   begin
					 let arr1 = Array.copy arr in
					 arr1.(j) <- WA.Cn(p,tt);
					 _stack := (qq1,arr1) :: !_stack;
					 if depends ddOrig p p then
					   let arr2 = Array.copy arr in
					   arr2.(j) <- WA.Hd [(p,tt)];
					   _stack := (qq1,arr2) :: !_stack;
					 else ()
				   end
				| WA.Hd qq2 ->
				   let (qq1,p1) = w1 in
				   if depends ddOrig p1 p then
					 let arr1 = Array.copy arr in
					 arr1.(j) <- WA.Hd ((p,tt)::qq2);
					 _stack := (qq1,arr1) :: !_stack;
				   else ()
				| WA.Cn _ -> ()
			  end
		   | _ -> failwith ""
		 done;
	   end
  done;
  !_res
;;	   

let updateWSH ddOrig (qq : WandHeader.t) (h : WSH.t) : WSH.t list =
  let gg = h.WSH.gm in
  let gg1 = L.filter (fun (s,_) -> match s with WS.Pr(_,_) -> true | _ -> false) gg in
  let attrLL = mkAllAttrsGG ddOrig qq gg1 in
  L.map (updateWSH_attrs h) attrLL
;;	

let updateWID ddOrig (d : WandIndDef.t) : WandIndDef.t =
  let (qq,p,prm,hh) = d in
  match L.flatten (L.map (updateWSH ddOrig qq) hh) with
  | [] ->
	 let n = IS.ptoSize ddOrig in
	 (qq,p,prm,[WSH.falseSH "%0" n])
  | hh1 -> (qq,p,prm,hh1)
;;

let updateWIS ddOrig (dd : WandIS.t) : WandIS.t = L.map (updateWID ddOrig) dd
;;

let updateIndSys ddwand ddOrig =
  let dd = WIS.create ddwand ddOrig in
  let dd1 = updateWIS ddOrig dd in
  WIS.down dd1
;;


(*------------------------------------------------------------------*)
(* Dropping Meaningless IndDef with Satchecking                     *)
(*------------------------------------------------------------------*)
let ccComputeBasePair ddWand =
  let ddSl = SlxSyntax.IS.forget (CcSatcheck.FromCCtoSLX.of_IS ddWand) in
  let (_,yy) = SlSatcheck.computeBasePair ddSl in
  yy
;;

let contradictIndPred ddWand =
  let yy = ccComputeBasePair ddWand in
  let contraBP = L.filter (fun (_,_,bp) -> bp = []) yy in
  L.map (fun (p,_,_) -> p) contraBP
;;

(* updateIndSys2 returns ddWand with contradict ind.pred. list *)
let updateIndSys2 ddwand ddOrig =
  let ddWand = updateIndSys ddwand ddOrig in
  let contraList = dropRed (contradictIndPred ddWand) in
  (ddWand,contraList)
;;

(*------------------------------------------------------------------*)
(* Inductive-Wands                                                  *)
(*------------------------------------------------------------------*)
(* A inductive-wand Q1(t1,t2),Q2(u)-* P(v1,v2) has the following forms *)
(* 1. ([("Q1",[t1;t2]);("Q2",[u])],P(v1,v2)) : wand specific form   *)
(* 2. Pr("Q1@2:Q2@1-*P", [v1;v2;u;t1;t2])  : usual SpatExp notation *)
(*------------------------------------------------------------------*)
let parseWand pr argLen = (* pr: "R@2:Q@1-*P", argLen: 4 *)
  let prP_prH = splitString "-*" pr in
  let prP = L.hd prP_prH in (* prP: "P" *)
  let prHL = L.tl prP_prH in (* prHL: ["R@2:Q@1"] *)
  let p_nL = L.flatten (L.map (splitString ":") prHL) in
  let npL = L.map (splitString "@") p_nL in
  let pnL = L.map (fun np -> (L.nth np 1,int_of_string (L.nth np 0))) npL in (* pnL: [("R",2);("Q",1)] *)
  let argLenP = argLen - (sum (L.map snd pnL)) in
  (prP,argLenP) :: pnL (* [("P",1);("Q",1);("R",2)]*)

module Wand = struct
	type t = (string * T.t list) list * (string * T.t list)

	(* The function wand converts the form 2 to the form 1 *)
	let fromSpatExp (s : SHspatExp.t) : t =
	  match s with
	  | S.Emp -> failwith "wandS2W: not wand predicate"
	  | S.Pto(_,_) -> failwith "wandS2W: not wand predicate"
	  | S.Pr(pr,tt) ->
		 let pnL = parseWand pr (L.length tt) in
		 let _hd = ref [] in
		 let _prm = ref tt in
		 let _top = ref [] in
		 for i = 0 to L.length pnL - 1 do
		   let (pr,argLen) = L.nth pnL i in
		   let (ttArg,rest) = getInitSeg !_prm argLen in
		   _prm := rest;
		   if i = 0 then _top := [(pr,ttArg)] else _hd := (pr,ttArg) :: !_hd
		 done;
		 (!_hd,L.hd !_top)

	let toSpatExp (wd : t) : SHspatExp.t =
	  let (ww,(pr,tt)) = wd in
	  let qq = L.map (fun (p,uu) -> p ^ "@" ^ (string_of_int (L.length uu))) ww in
	  let pr1 =
		if qq = [] then pr
		else (string_of_list (fun x -> x) ":" qq)^ "-*" ^ pr
	  in
	  let uu = L.flatten (tt :: (L.map snd (L.rev ww))) in
	  S.Pr(pr1,uu)

end
