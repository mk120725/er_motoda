(* Brotherston's satisfiability checker *)
open SlSyntax;;
open Tools;;

module OptSat = Options.Satchecker

exception Unsat_Found;;

module UnfoldTr = struct
(* Types of Unfolding trees *)
(* Tr(P,1,[tr1;tr2]) means P is unfolded by the 1st-rule of P, *)
(* and tr1 and tr2 are subtrees *)
  type t = Tr of string * int * t list

  let tr(pr,n,trL) = Tr(pr,n,trL)

  let rec size (tr : t) =
    let Tr(_,_,trL) = tr in
    let n = List.fold_right (fun x->fun y-> (size x)+y) trL 0 in
    n+1
	
  let rec to_string (tree : t) =
    let Tr(pr,n,trL) = tree in
    let strtrL = if trL = [] then "[]"
		 else 
		   List.fold_right
		     (fun x->fun y-> if y = "" then x else x^","^y)
		     (List.map to_string trL) ""
    in "("^pr^","^(string_of_int n)^","^strtrL^")"

end;;
    
module BasePair = struct
(* Types of BasePairs*)
  type t = string list * SHpure.t * UnfoldTr.t

  let nf (bp : t) : t = 
    let module P = SHpure in
    let module Px = SHpureExp in
    let (v,p,t) = bp in
    let p' = dropRedSorted (=) (P.sort (List.map Px.nf p)) in
    (v, p',t)

  let order (bp1 : t) (bp2 : t) = 
    let (_,p1,_) = bp1 in
    let (_,p2,_) = bp2 in
    lexorder SHpureExp.order p1 p2
    
  let strongorder (bp1 : t) (bp2 : t) = 
    let (_,p1,tr1) = bp1 in
    let (_,p2,tr2) = bp2 in
    let s1 = UnfoldTr.size tr1 in
    let s2 = UnfoldTr.size tr2 in
    let pcomp = lexorder SHpureExp.order p1 p2 in
    if  pcomp = 0 && s1 > s2 then 1
    else if pcomp = 0 && s1 < s2 then -1
    else if pcomp = 0 && s1 = s2 then 0
    else pcomp
      
  let equal bp1 bp2 = if order bp1 bp2 = 0 then true else false

  let rec equalL bpL1 bpL2 = match bpL1,bpL2 with
    | bp1::bpL1',bp2::bpL2' -> if equal bp1 bp2 then equalL bpL1' bpL2'
			       else false
    | [],[] -> true
    | _,_ -> false
							    
  let to_string (it : t) = 
    let (vars,pure,_) = it in
    let var_str = concatStrLComma vars in
    let pure_str = concatStrLComma (List.map SHpureExp.to_string pure) in
    "({"^var_str^"}, {"^pure_str^"})"

  let print (is : t) = print_string ((to_string is)^"\n")

  let subst sub (bp : t) : t list =
    let module T = SHterm in
    let subst1 sub s = match findItemOption s sub with
      | None -> []
      | Some tm -> match tm with
		   | T.Nil -> []
		   | T.Var v -> [v]
    in
    let substMany sub vars =
      let res = List.concat ((List.map (subst1 sub)) vars) in
      if List.length res <> List.length vars then None else Some res
    in
    let (v,p,t) = bp in
    match substMany sub v with
    | None -> []
    | Some v' ->
       let p' = SHpure.subst sub p in
       [(v',p',t)]
				    
end;;

(* Tensor in Brotherston's paper *)
let tensor tt = 
  let module P = SHpureExp in
  let module T = SHterm in
  let combpairL = makeCombPairs tt in
  let neqTerms = List.map (fun (x,y) -> P.Neq(x,y)) combpairL in
  let neqNil = List.map (fun t -> P.Neq(t,T.Nil)) tt in
  neqTerms@neqNil;;

let tensor2 vars1 vars2 = 
  let module P = SHpureExp in
  let module T = SHterm in
  let varTm1 = List.map (fun x -> SHterm.Var x) vars1 in
  let varTm2 = List.map (fun x -> SHterm.Var x) vars2 in
  let combLL = List_tailrec.allChoice [varTm1;varTm2] in
  let combpairL = List.map (fun ls -> (List.nth ls 0,List.nth ls 1)) combLL in
  let neqVars = List.map (fun (x,y) -> P.Neq(x,y)) combpairL in
  SHpure.sort neqVars;;

(* Canonical variable				*)
(* canonical pure vars torder y		*)
(* returns the canonical variable of y w.r.t. pure with vars *)
module Eqpair = struct
  type t = SHterm.t * SHterm.t
end;;

module TermRepl = struct

  let of_tm1_ord order (eq : Eqpair.t) tm =
    let module T = SHterm in
    let (t,u) = eq in
    if t = u then tm
    else if t <> tm && u <> tm then tm
    else match t,u with
	 | (T.Nil,_) -> T.Nil
	 | (_,T.Nil) -> T.Nil
	 | (T.Var x,T.Var y) -> if order x y > 0 then u else t

  let rec update_eq_ord order (eq : Eqpair.t) (eql : Eqpair.t list) : Eqpair.t list = 
  match eql with
    | [] -> []
    | (t,u)::eql1 -> 
       let t' = of_tm1_ord order eq t in 
       let u' = of_tm1_ord order eq u in 
       let eql' = update_eq_ord order eq eql1 in
       if t' = u' then eql' else (t',u')::eql'

  let rec of_tm_ord order eql tm = match eql with
    | [] -> tm
    | eq::eql1 -> 
       let tm' = of_tm1_ord order eq tm in
       let eql1' = update_eq_ord order eq eql1 in
       of_tm_ord order eql1' tm'

  let rec of_pure_ord order eq (pure : SHpure.t) : SHpure.t =
    let module P = SHpureExp in
    match pure with
    | [] -> []
    | P.Eq(t,u)::pure' -> 
       let t1 = of_tm_ord order eq t in
       let u1 = of_tm_ord order eq u in
       let pure1 = of_pure_ord order eq pure' in
       P.Eq(t1,u1)::pure1
    | P.Neq(t,u)::pure' -> 
       let t1 = of_tm_ord order eq t in
       let u1 = of_tm_ord order eq u in
       let pure1 = of_pure_ord order eq pure' in
       P.Neq(t1,u1)::pure1

  let rec of_spat_ord order eq (spat : SHspat.t) : SHspat.t =
    let module S = SHspatExp in
    match spat with
    | [] -> []
    | S.Emp::spat' -> S.Emp::(of_spat_ord order eq spat')
    | S.Alloc(t,tL)::spat' -> 
       let t1 = of_tm_ord order eq t in
       let tL1 = List.map (of_tm_ord order eq) tL in
       let spat1 = of_spat_ord order eq spat' in
       S.Alloc(t1,tL1)::spat1
    | S.Ind(pr,tL)::spat' -> 
       let tL1 = List.map (of_tm_ord order eq) tL in
       let spat1 = of_spat_ord order eq spat' in
       S.Ind(pr,tL1)::spat1

  let of_tm = of_tm_ord strlexorder
			
  let update_eq = update_eq_ord strlexorder
				
  let of_pure = of_pure_ord strlexorder
			
end;;

let rec splitEqPure (pure : SHpure.t) : Eqpair.t list * SHpure.t = 
match pure with
  | [] -> ([],[])
  | SHpureExp.Eq(t,u)::pure' -> 
     let (eqL,neqL) = splitEqPure pure' in ((t,u)::eqL,neqL)
  | neq::pure' -> let (eqL,neqL) = splitEqPure pure' in (eqL,neq::neqL);;

let rec splitEqPureSym (pure : SHpure.t) sym : Eqpair.t list * SHpure.t * SHpure.t =
  let module T = SHterm in
  let module P = SHpureExp in
  match pure with
  | [] -> ([],[],[])
  | P.Eq(t,u)::pure' when t = u -> splitEqPureSym pure' sym
  | P.Eq(t,u)::pure' ->
     let eq = SHpureExp.Eq(t,u) in
     let (eqpL,eqL,neqL) = splitEqPureSym pure' sym in
     begin
       match t with
       | T.Var v -> 
	  if sym = String.get v 0 then ((t,u)::eqpL,eqL,neqL)
	  else (eqpL,eq::eqL,neqL)
       | _ -> (eqpL,eq::eqL,neqL)
     end
  | neq::pure' -> let (eqpL,eqL,neqL) = splitEqPureSym pure' sym in (eqpL,eqL,neq::neqL);;    

let canonicalTerm pure xbase t = 
  let module T = SHterm in
  let (eql,_) = splitEqPure pure in
  let xbaseTm = List.map (fun x -> T.Var x) xbase in
  if List.mem t (T.Nil::xbaseTm) then t else 
  TermRepl.of_tm eql t;;

let canonicalPure pure xbase pex = 
  let module P = SHpureExp in
  match pex with
  | P.Eq(t,u) ->
     let ct = canonicalTerm pure xbase t in
     let cu = canonicalTerm pure xbase u in
     P.Eq(ct,cu)
  | P.Neq(t,u) ->
     let ct = canonicalTerm pure xbase t in
     let cu = canonicalTerm pure xbase u in
     P.Neq(ct,cu);;

(* alloced(V,x,Pi) in Brotherston's paper *)
let alloced pure xbase vars =
  let module T = SHterm in  
  let vTm = List.map (canonicalTerm pure xbase) vars in
  let vars' = T.extractVars vTm in
  List.filter (fun x -> List.mem x xbase) vars' 

(* restriction of Pi with [x] *)
let restrictPure pure xbase : SHpure.t = 
  let module T = SHterm in
  let module P = SHpureExp in
  let filterPure tmL pex = match pex with
    | P.Eq(t,u) | P.Neq(t,u) ->
		   if List.mem t tmL && List.mem u tmL then true else false
  in
  let pure' = List.map (canonicalPure pure xbase) pure in
  let xbaseTm = List.map (fun x -> T.Var x) xbase in
  List.filter (filterPure (T.Nil::xbaseTm)) pure';;

(* Checking Satisfiability of Pure-expressions *)
(*
let checkSatPure pure = 
  let module T = SHterm in
  let module P = SHpureExp in
  let (eqpairs,pureNeq) = splitEqPure pure in
  let dropTriv eqps = List.filter (fun (t,u)-> t <> u) eqps in
  let rec findUnsat pures = match pures with
    | [] -> ()
    | P.Eq(_,_)::pures' -> findUnsat pures'
    | P.Neq(t,u)::pures' -> if t = u then raise Unsat_Found
			 else findUnsat pures'
  in	    
  let rec chsatpureR eqps neql = match eqps with
    | [] -> true
    | eq::eqps' ->
       let eqps'' = dropTriv (TermRepl.update_eq eq eqps') in
       let neql'' = TermRepl.of_pure [eq] neql in
       findUnsat neql'';
       chsatpureR eqps'' neql''
  in
  try chsatpureR eqpairs pureNeq with Unsat_Found -> false;;
*)
let checkSatPure pure =
  let module T = SHterm in
  let module P = SHpureExp in
  let (eqpairs,pureNeq) = splitEqPure pure in
  let rec substEqs eqps p =
    match eqps with
    | [] -> p
    | (t,u)::eqps1 ->
      let sub =
	match t,u with
	| T.Nil,T.Nil -> []
	| T.Var x,_ -> [(x,u)]
	| _,T.Var x -> [(x,t)]
      in
      let eqps1' =
	List.map
	(fun (t,u) -> (T.subst sub t,T.subst sub u)) eqps1 in
      let p' = List.map (P.subst sub) p in
      substEqs eqps1' p'
  in 
  let rec checkSatNeq pNeq =
    match pNeq with
    | [] -> true
    | P.Neq(t,u)::pNeq' when t <> u -> checkSatNeq pNeq'
    | P.Neq(t,u)::pNeq' -> false
    | _::pNeq' -> checkSatNeq pNeq' (* dummy *)
  in
  checkSatNeq (substEqs eqpairs pureNeq)
;;

module Yvec = struct
  (* [(P,[%0;%1],[bpP1,bpP2]); (Q,[%0],[bpQ1,bpQ2,bpQ3])] *)
  type t = (string * string list * BasePair.t list) list

  let getInfo (yy : t) p = match List.filter (fun (q,_,_) -> p = q) yy with
    | [] -> []
    | (_,prm,bpL)::_ -> [(prm,bpL)]

  let getBasePair (yy : t) p = match List.filter (fun (q,_,_) -> p = q) yy with
    | [] -> []
    | (_,_,bpL)::_ -> bpL

  let appOne (yy : t) spex = 
    let module S = SHspatExp in 
    match spex with
    | S.Ind(pr,tmL) ->
       begin
	     match getInfo yy pr with
	     | [] -> []
	     | (prms,bpL)::_ ->
	        if List.length tmL <> List.length prms then []
	        else List.concat (List.map (BasePair.subst (zipLst prms tmL)) bpL)
       end
    | _ -> []

  let apply yy (spat : SHspat.t) = List.map (appOne yy) spat

  let merge is yy1 yy2 =
    let merge1 y1 y2 pr =
      let (prm,bpL1) = List.hd (getInfo y1 pr) in
      let (_,bpL2) = List.hd (getInfo y2 pr) in
      let bpL = List.fast_sort BasePair.order bpL1@bpL2 in
      (pr,prm,bpL)
    in
    List.map (merge1 yy1 yy2) (IS.preds is)

  let equal (is : IS.t) (yy1 : t) (yy2 : t) = 
    let module T = SHterm in
    let prlst = List.map (fun (pr,_,_) -> pr) is in
    List.for_all
      (fun pr ->
       BasePair.equalL (getBasePair yy1 pr) (getBasePair yy2 pr)
      ) prlst

  let init (is : IS.t) : t = List.map (fun (pr,prm,_) -> (pr,prm,[])) is
				       
  let to_string (is : IS.t) (yy : t) = 
    let module T = SHterm in
    let prlst = List.map (fun (pr,prm,_) -> (pr,prm)) is in
    let bplistbody pr = List.map BasePair.to_string (getBasePair yy pr) in
    let bplist (pr,prm) = "("^pr^", ["^(concatStrLComma prm)^"], "^(concatStrLComma (bplistbody pr))^")" in
    (concatStrLNewLine (List.map bplist prlst))^"\n"

  let print (is : IS.t) (yy : t) = print_string (to_string is yy)

end;;

let rec dropTrivEq pure = 
  let module P = SHpureExp in
  let module T = SHterm in
  match pure with
  | [] -> []
  | hd::pure' -> 
	match hd with
	| P.Eq(t,u) -> if t = u then dropTrivEq pure' else hd::(dropTrivEq pure')
	| _ -> hd::(dropTrivEq pure');;


let funcIdef is pr n (yy : Yvec.t) : BasePair.t list = 
  let module T = SHterm in
  let makeHdBasepair syhp =
    let (pu,sp) = syhp in    
    let allocL = SHspat.allocTerms sp in
    let vars = SHterm.extractVars allocL in
    let pure = (tensor allocL)@pu in
    (vars,pure,[])
  in
  let mergeBasepair bpt1 bpt2 =
    let (v1,p1,trL1) = bpt1 in
    let (v2,p2,trL2) = bpt2 in
    let v = v1@v2 in
    let p = (tensor2 v1 v2)@p1@p2 in
    (v,p,trL1@trL2)
  in
  match IS.nthRule is pr n with
  | None -> []
  | Some (_, prm, ruleSH) ->
     let (pure,spat) = ruleSH in
     let (allc,ind) = SHspat.split spat in
     let basepairLL = Yvec.apply yy ind in
     let mkbpL (v,p,t) = (v,p,[t]) in
     let basepairtLL = List.map (fun bpL -> List.map mkbpL bpL) basepairLL in
     let result = ref [] in
     List_tailrec.allChoiceApply1
       (fun bptL ->	(* last function *)
        let (vars,pure,trL) = List.hd bptL in
	let vTm = List.map (fun x -> T.Var x) vars in
	let vars' = alloced pure prm vTm in
	let pure' = restrictPure pure prm in
	let tr = UnfoldTr.tr(pr,n,List.rev trL) in
	(*	let tr = UnfoldTr.tr(pr,n,trL) in*)
	result := (vars',pure',tr)::!result)
       (fun bptL ->	(* condition function *)
	let bpt0 = List.nth bptL 0 in
  	let bpt1 = List.nth bptL 1 in
	let (vars,pure,trL) = mergeBasepair bpt0 bpt1 in
	let vars' = List.fast_sort strlexorder vars in
	let pure' = restrictPure pure prm in
	if not(isLinearSorted vars')
	then raise Skip
	else if checkSatPure pure'
	then
  	  let vTm = List.map (fun x -> T.Var x) vars' in
	  let vars'' = alloced pure prm vTm in
  	  let pure'' = dropRedSorted (=) (dropTrivEq (SHpure.sort pure')) in
	  [(vars'',pure'',trL)] else raise Skip
       )
       basepairtLL
       [makeHdBasepair ruleSH]; !result;;

let funcY1 is (yy : Yvec.t) pr : Yvec.t =
  let module T = SHterm in
  let module BP = BasePair in
  match IS.rule_of is pr with
  | None -> []
  | Some(_,prm,defL) -> 
     let n = List.length defL in
     let numL = List.rev (genLst n) in
     let bpL = List.concat (List.map (fun i -> funcIdef is pr i yy) numL) in
     let bpL_nf = List.map BP.nf bpL in
     let bpL_sorted = dropRedSorted BP.equal (List.fast_sort BP.strongorder bpL_nf) in
     [(pr, prm, bpL_sorted)];;

  
let funcY is yy : Yvec.t = 
  let prlst = IS.preds is in
  List.concat (List.map (funcY1 is yy) prlst);;

  
let computeBasePair is = 
  let cnt = ref 0 in
  let rcd = ref "" in
  let addcnt () = cnt := !cnt + 1 in
  let addrec yy =
    let yy_string = (Yvec.to_string is yy) in
    addcnt ();
    rcd := !rcd^"\n\nCounter : "^(string_of_int !cnt)^"\n"^yy_string
  in
  let yy0 = ref (Yvec.init is) in
  let yy1 = ref (funcY is !yy0) in
  addrec !yy0;
  while (not (Yvec.equal is !yy0 !yy1)) do
    addrec !yy1;
    yy0 := !yy1;
    yy1 := funcY is !yy1;
  done;
  let yy_result = !yy1 in
  (!rcd, yy_result)
;;


exception Evidence_Found of (string list * SHpure.t * UnfoldTr.t) ;;  

let satcheckCore checkBPT (yy : Yvec.t) (sh : SH.t) =
  let module U = UnfoldTr in
  let makeHdBasepair syhp =
    let (pu,sp) = syhp in
    let allocL = SHspat.allocTerms sp in
    let vars = SHterm.extractVars allocL in
    let pure = (tensor allocL)@pu in
    (vars,pure,[])
  in
  let mergeBasepair bpt1 bpt2 =
    let (vv1,pp1,tL1) = bpt1 in
    let (vv2,pp2,tL2) = bpt2 in
    let vv = mergeL strlexorder [vv1;vv2] in
    let pp = (tensor2 vv1 vv2)@pp1@pp2 in
    (vv,pp,tL1@tL2)
  in
  let (pure,spat) = sh in
  let (allc,ind) = SHspat.split spat in
  let mkbpL (vv,pp,t) = (vv,pp,[t]) in
  let basepairLL = Yvec.apply yy ind in
  let basepairtLL = List.map (List.map mkbpL) basepairLL in
  try 
    List_tailrec.allChoiceApply1
      (fun bptL ->	(* last function *)
		match bptL with
		| [] -> ()
		| (vv,pp,trL)::_ ->
		   if checkBPT (vv,pp,trL) && checkSatPure pp
		   then
			 let tr = U.Tr("GOAL",0,List.rev trL) in
			 raise (Evidence_Found (vv,pp,tr))
		   else ()
	  )
      (fun bpL ->	(* condition function *)
       let (vars,pure,trL) = mergeBasepair (List.nth bpL 0) (List.nth bpL 1) in
       if not(isLinearSorted vars)
       then raise Skip
       else if checkSatPure pure
       then [(vars,pure,trL)] else raise Skip
      )
      basepairtLL
      [makeHdBasepair sh]; None
  with (Evidence_Found(vv1,pp1,t)) -> Some(vv1,pp1,t)
;;

let satcheck yy sh =
  let checkBPT (_,p,_) = checkSatPure p in
  satcheckCore checkBPT yy sh
;;

let satcheckBool (yy : Yvec.t) (sh : SH.t) =
  match satcheck yy sh with
  | None -> false
  | Some(_,_,_) -> true
;;

(*-------------------------------------*)
(* Making A Model from Unfolding Tree  *)
(*-------------------------------------*)
 let rec unfoldIndPred is vars tr pr tmL =
  let module T = SHterm in
  let module U = UnfoldTr in
  let U.Tr(pr',n,trL) = tr in
  if pr <> pr' then None else 
  match IS.rule_of is pr with
  | None -> None
  | Some(_,prm,ruleSHL) ->
     if List.length ruleSHL < n + 1 then None
     else
       let sh = List.nth ruleSHL n in
       let vars1 = SH.allvars sh in
       let bv1 = List.filter (fun x -> not(List.mem x prm)) vars1 in
       let bv_sorted = List.fast_sort strlexorder bv1 in
       let bv = dropRedSorted (=) bv_sorted in
       let n = List.length bv in
       let fvTm = List.concat (List.map SHterm.fv tmL) in
       let vars2 = List.rev_append fvTm (List.rev_append vars1 vars) in
       let freshVars = genFreshVarL "$" vars2 n in
       let freshVarsTm = List.map (fun x -> T.Var x) freshVars in
       let sub1 = zipLst bv freshVarsTm in
       let sub2 = zipLst prm tmL in
       let sub = List.rev_append sub1 sub2 in
       let (pure,spat) = SH.subst sub sh in
       let vars3 = List.rev_append freshVars vars2 in
       let vars3_sorted = List.fast_sort strlexorder vars3 in
       let vars4 = dropRedSorted (=) vars3_sorted in
       unfoldSH1 is vars4 trL pure spat []
and unfoldSH1 is vars treeL pure spat rest =
  let module S = SHspatExp in
  let module Unfold = UnfoldTr in
  match spat,treeL with
  | [],[] -> Some(pure,List.rev rest,vars)
  | [],_ -> None
  | S.Emp::spat1,_ -> unfoldSH1 is vars treeL pure spat1 rest
  | S.Alloc(t,tL)::spat1,_ ->
     unfoldSH1 is vars treeL pure spat1 (S.Alloc(t,tL)::rest)
  | S.Ind(_,_)::_,[] -> None
  | S.Ind(pr,tmL)::spat1,tr::treeL1 ->
     match unfoldIndPred is vars tr pr tmL with
     | None -> None
     | Some(pure2,spat2,vars2) ->
	let pure3 = List.rev_append pure2 pure in
	let rest3 = List.rev_append spat2 rest in
	let vars3 = dropRedSorted (=) (List.fast_sort strlexorder vars2) in
	unfoldSH1 is vars3 treeL1 pure3 spat1 rest3
;;
  
let createModel is (sh : SH.t) tr : SH.t option =
  let module U = UnfoldTr in
  let U.Tr(_,_,tL) = tr in
  let (pure,spat) = sh in
  let vars = List.fast_sort strlexorder (SH.allvars sh) in
  let vars' = dropRedSorted (=) vars in
  match unfoldSH1 is vars' tL pure spat [] with
  | None -> None
  | Some(pure',spat',_) -> Some(pure',spat')
;;


(*--------------------------------------*)
(* Deciding SH                          *)
(*--------------------------------------*)
let decideSat is sh =
  OptSat.doifDebug print_endline "** SlSymbolic Heap";
  OptSat.doifDebug SlSyntax.SH.println sh;
  OptSat.doifDebug print_endline "---------";  
  let (record,yy) = computeBasePair is in
  match satcheck yy sh, OptSat.modelFlag () with
  | None,_ -> (None,record)
  | Some(vv,pp,tr),true ->
	 let model = createModel is sh tr in
	 (Some(vv,pp,model),record)
  | Some(vv,pp,tr),false -> (Some(vv,pp,None),record)
 ;;
