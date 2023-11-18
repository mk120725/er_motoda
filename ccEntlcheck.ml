open Tools
open CcSyntax
open CcWand

module Opt = Options.Cycomp;;
module Lt = List_tailrec;;


(*--------------------------------------------------*)  
(* Reducing Entailments by Checking Grouping        *)
(*--------------------------------------------------*)  
let elimExvarsPP vvEx ppEq ppNq =
  (* elimExvarsPP z [x=z;a=x] [w<>y;z<>y] means   *)
  (* Ex z (x=z & a=x & w<>y & z<>y)             *)
  (* It returns a=x & w<>y & x<>y               *)
  let ttEx = L.map var vvEx in
  let rec aux ppEq2 ppNq2 ppEqIn =
	match ppEqIn with
	| [] ->
	   let ppNq3 = L.filter (fun (_,t,u) -> not(L.mem t ttEx || L.mem u ttEx)) ppNq2 in
	   ppEq2 @ ppNq3
	| (_,T.Var v,u)::ppEqIn' when L.mem v vvEx ->
	   let ppEqIn'' = PP.subst [(v,u)] ppEqIn' in
	   let ppNq2' = PP.subst [(v,u)] ppNq2 in
	   aux ppEq2 ppNq2' ppEqIn''
	| (_,u,T.Var v)::ppEqIn' when L.mem v vvEx ->
	   let ppEqIn'' = PP.subst [(v,u)] ppEqIn' in
	   let ppNq2' = PP.subst [(v,u)] ppNq2 in
	   aux ppEq2 ppNq2' ppEqIn''
	| q::ppEqIn' ->
	   aux (q::ppEq2) ppNq2 ppEqIn'
  in
  aux [] ppNq ppEq
	
let extractGrpPP (h : SH.t) =
  let vvEx = h.SH.ex in
  let (ppEq0,ppNq0) = PP.splitEqNeq h.SH.pi in
  let (pairEq,gps) = GG.extractGrp h.SH.gm in
  let ppEq1 = L.map (fun (t,u) -> t =.= u) pairEq in
  let _ppNq = ref [] in
  for i = 0 to L.length gps - 1 do
	let ttGi = L.nth gps i in
	for j = i+1 to L.length gps - 1 do
	  let ttGj = L.nth gps j in
	  _ppNq := (mkNeqAll ttGi ttGj) @ !_ppNq
	done;
  done;
  elimExvarsPP vvEx (ppEq0 @ ppEq1) (ppNq0 @ !_ppNq)
  

let checkByGroup h1 h2 =
  (* It checks the cell conditions of h1 and h2 can be coexists *)
  (* It it fails, h1 |- h2,hh can be reduced to h1 |- hh        *)
  let ppCell1 = extractGrpPP h1 in
  let ppCell2 = extractGrpPP h2 in
  PP.satcheck (ppCell1 @ ppCell2)

let reduceByCheckingGroup e =
  let e1 = Entl.clone e in
  let (_,h,hh) = Entl.decomp e1 in
  let hh1 = L.filter (checkByGroup h) hh in
  e1.Entl.suc <- hh1;
  e1

let trivCheck e =
  let (_,h,hh) = Entl.decomp (Entl.nf e) in
  let gg = h.SH.gm in
  let ggg = L.map (fun h -> h.SH.gm) hh in
  L.mem gg ggg

let reduceByCheckingDuplicatedRoot e =
  let rec checkLinearSorted ls =
	match ls with
	| [] -> true
	| x::[] -> true
	| x::y::ls1 -> if x = y then false else checkLinearSorted (y::ls1)
  in
  let checkSH h =
	let roots = L.sort T.compare (SH.getRoots h) in
	if checkLinearSorted roots then true else false
  in
  let e1 = Entl.clone e in
  e1.Entl.suc <- L.filter checkSH e1.Entl.suc;
  e1

let reduceByCompareRootCells e =
  let e1 = Entl.clone e in
  let ttRootCellsL = SH.getRootCells e.Entl.ant in
  let checkSH h = subset (SH.getRootCells h) ttRootCellsL in
  e1.Entl.suc <- L.filter checkSH e1.Entl.suc;
  e1

let checkByPointsToSH h1 h2 =
  let rec findPtoHd t gg =
    match gg with
    | [] -> None
    | (S.Pto(u,uu),_)::gg1 when t = u -> Some uu
    | _::gg1 -> findPtoHd t gg1
  in
  let rec aux gg1 gg2 =
    match gg2 with
    | [] -> []
    | (S.Pto(t,tt),_)::gg2' ->
       begin
         match findPtoHd t gg1 with
         | Some uu -> (tt =..= uu) @ (aux gg1 gg2')
         | None -> aux gg1 gg2'
       end
    | (_,_)::gg2' -> aux gg1 gg2'
  in
  let pp = h1.SH.pi in
  let ppGG = aux h1.SH.gm h2.SH.gm in
  SHpure.entlcheck pp [ppGG]
       
let reduceByCheckingPointsTo e =
    let e1 = E.clone e in
    e1.E.suc <- L.filter (checkByPointsToSH e1.E.ant) e1.E.suc;
    e1


(*-----------------------------------*)
(* eliminating right allocated Emp   *)
(*-----------------------------------*)
let elimRightAllocatedEmp e = 
  let e1 = E.clone e in
  let hh = e1.E.suc in
  let _hh = ref [] in
  let _gg = ref [] in  
  for i = 0 to List.length hh - 1 do
    let h = List.nth hh i in
    let gg = h.SH.gm in
    for j = 0 to List.length gg - 1 do
      let g = L.nth gg j in
	  match g with
	  | S.Emp,allocs when allocs <> [] -> ()
      | _ ->
         _gg := g::!_gg
    done;
    h.SH.gm <- List.rev !_gg;
    _hh := h :: !_hh;
  done;
  e1.E.suc <- List.rev !_hh;
  e1    
;;    
    
(*-----------------------------------*)
(* Match(J)                          *)
(*-----------------------------------*)
let elimLeftEq e =
  (* (Match-1) Equality Elimination *)
  (* x = t && F |- G becomes F[x:=t] |- G[x:=t] *)
  let e1 = E.clone e in
  let pp = e1.E.ant.SH.pi in
  let h = e1.E.ant in
  for i = 0 to L.length pp - 1 do
	match L.nth pp i with
	| Neq,_,_ -> ()
	| Eq,T.Var v,u | Eq,u,T.Var v ->
	   begin
		 h.SH.up <- L.map (T.subst [(v,u)]) h.SH.up;
		 h.SH.out <- L.map (T.subst [(v,u)]) h.SH.out;
		 h.SH.pi <- L.map (P.subst [(v,u)]) h.SH.pi;
		 h.SH.gm <- L.map (G.subst [(v,u)]) h.SH.gm;
		 e1.E.suc <- L.map (SH.subst [(v,u)]) e1.E.suc;
	   end
	| Eq,_,_ -> ()
  done;
  e1.E.ant.SH.pi <- L.filter (fun (op,_,_) -> op = Neq) e1.E.ant.SH.pi;
  E.nf e1
;;


let matchPto e =
  (* (Match-2) Match *)
  (* x -> (a) * y -> (b) |- x -> (p) | y -> (q) *)
  (* becomes x -> (a) |- a = p && x -> (p) | b = q && y -> (q) *)
  let e1 = E.clone e in
  let gg = e1.E.ant.SH.gm in
  for i = 0 to L.length gg - 1 do
	match fst(L.nth gg i) with
	| S.Pto(t,tt) ->
	   let hh = e1.E.suc in
	   for j = 0 to L.length hh - 1 do
		 let gg1 = (L.nth hh j).SH.gm in
		 for k = 0 to L.length gg1 - 1 do
		   match fst(L.nth gg1 k) with
		   | S.Pto(u,uu) when t = u -> 
			  (L.nth hh j).SH.pi <- (tt =..= uu) @ (L.nth hh j).SH.pi
		   | _ -> ()
		 done;
	   done;
	| _ -> ()
  done;
  e1
;;

let instEx e =
  (* (Match-3) Existential Instantiation *)
  (* F |- Ex zw(z = t & G) becomes F |- Ex w(G[z:=t]) *)
  let rec auxrec vv out up res pp gg =
	match pp with
	| [] -> {SH.ex=vv; SH.out=out; SH.up=up; SH.pi=L.rev res; SH.gm=gg}
	| p::pp' ->
	   match p with
	   | (Eq,T.Var v,u) when L.mem v vv ->
		  let vv1 = L.filter (fun x -> x<>v) vv in
		  let out1 = L.map (T.subst [(v,u)]) out in
		  let up1 = L.map (T.subst [(v,u)]) up in
		  let pp1 = PP.subst [(v,u)] pp' in
		  let res1 = PP.subst [(v,u)] res in
		  let gg1 = GG.subst [(v,u)] gg in
		  auxrec vv1 out1 up1 res1 pp1 gg1
	   | (Eq,u,T.Var v) when L.mem v vv ->
		  let vv1 = L.filter (fun x -> x<>v) vv in
		  let out1 = L.map (T.subst [(v,u)]) out in
		  let up1 = L.map (T.subst [(v,u)]) up in
		  let pp1 = PP.subst [(v,u)] pp' in
		  let res1 = PP.subst [(v,u)] res in
		  let gg1 = GG.subst [(v,u)] gg in
		  auxrec vv1 out1 up1 res1 pp1 gg1	
	   | _ -> auxrec vv out up (p::res) pp' gg
  in
  let aux h = auxrec h.SH.ex h.SH.out h.SH.up [] h.SH.pi h.SH.gm in
  let e1 = E.nf (E.clone e) in
  e1.E.suc <- L.map aux e1.E.suc;
  E.nf e1
;;
  
let elimIncons e =
  (* (Match-4) Inconsistent Disjunct Elimination *)
  (* Out(x) && Up(y) && Pi && Gm |- G1 & Dn(x) | G2 & Dn(y) | G *)
  (* becomes Out(x) && Up(y) && Pi && Gm |- G *)
  let e1 = E.clone e in
  let vv = e1.E.ant.SH.out @ e1.E.ant.SH.up in
  let isCons k = disjoint vv (L.flatten ((L.map snd k.SH.gm))) in
  e1.E.suc <- L.filter isCons e1.E.suc;
  e1
;;	  

let isTrivEq (op,t,u) = (op = Eq && t = u)
;;

let isNonTrivEq (op,t,u) = (op = Eq && t <> u)
;;

let dropTrivEq h =
  h.SH.pi <- L.filter (fun p -> not (isTrivEq p)) h.SH.pi;
  h

let reduceByRightEq e =
  (* (Match-5) Unmatch Disjunct Elimination *)
  (* F |- t = u && G | G' becomes F |- G | G' if t \equiv u, F |- G' otherwise *)
  let e1 = E.clone e in
  let hasNonTrivEq h = (L.exists isNonTrivEq h.SH.pi) && h.SH.ex = [] in
  e1.E.suc <- L.filter (fun h -> not (hasNonTrivEq h)) e1.E.suc;
  e1.E.suc <- L.map dropTrivEq e1.E.suc;
  e1
;;

let elimDuplPure e =
  let elimDuplPureSH h =
    let h1 = SH.clone h in
    h1.SH.pi <- dropRed h1.SH.pi;
    h1
  in
  let e1 = E.clone e in
  e1.E.ant <- elimDuplPureSH e1.E.ant;
  e1.E.suc <- L.map elimDuplPureSH e1.E.suc;
  e1
;;  

let reduceByRightNeq e =
  (* (Match-6) Disequality Elimination *)
(* F |- t <> u && G | G' becomes F |- G' if t \equiv u, F |- G | G' otherwise *)
  let e1 = E.clone e in
  let dropDiffNeq k =
	k.SH.pi <- L.filter (fun (op,t,u) -> op = Eq || t = u) k.SH.pi;
	k
  in
  let isCons k = L.filter (fun (op,t,u) -> op = Neq && t = u) k.SH.pi = [] in
  e1.E.suc <- L.filter isCons e1.E.suc;
  e1.E.suc <- L.map dropDiffNeq e1.E.suc;
  e1
;;

let elimRightAllocatedEmp e =
  (* (Match-7) Emp Disjunct Elimination *)
  (* F |- G * Emp&Dn(x) | G' becomes F |- G' *)
  let e1 = E.clone e in
  let isCons k = L.filter (fun (g,xx) -> g = S.Emp && xx <> []) k.SH.gm = [] in
  e1.E.suc <- L.filter isCons e1.E.suc;
  e1
;;

let elimUnleaf e =
  (* (Match-8) Unleaf Elimination *)
  (* F |- Out(y) && G * x->(y) | G' becomes F |- G' *)
  let e1 = E.clone e in
  let isCons k =
	let leaves = L.flatten (L.map (fun (g,_) -> match g with S.Pto(_,tt) -> tt | _ -> []) k.SH.gm) in
	disjoint leaves k.SH.out
  in
  e1.E.suc <- L.filter isCons e1.E.suc;
  e1
;;

let getPtoRoots k =
  let getPtoRootOne g =
	match fst g with
	| S.Pto(t,_) -> [t]
	| _ -> []
  in
  L.flatten (L.map getPtoRootOne k.SH.gm)
;;

let getAllRoots k =
  let getRootOne g =
	match fst g with
	| S.Pto(t,_) -> [t]
	| S.Pr(_,t::_) -> [t]
	| _ -> []
  in
  L.flatten (L.map getRootOne k.SH.gm)
;;

let findCommon getRoots e =
  let roots = ref (getRoots e.E.ant) in
  for i = 0 to L.length e.E.suc - 1 do
	roots := intersect !roots (getRoots (L.nth e.E.suc i))
  done;
  !roots
;;

let findCommonPtoRoots = findCommon getPtoRoots;;
let findCommonAllRoots = findCommon getAllRoots;;

let removePto e =
(* (Match-9) pto Removal *)
(* F * x -> (y) |- Ex w(x->(z) * G1) | Ex w(x->(v) * G2) becomes *)
(* Up(x) && F |- Ex w G1 | Ex w G2 *)
  let e1 = E.clone e in
  let roots = findCommonPtoRoots e1 in
  let isNotThePto t g = 
	match fst g with
	|  S.Pto(u,_) -> if t = u then false else true
	| _ -> true
  in
  let removeThePto t =
	e1.E.ant.SH.gm <- L.filter (isNotThePto t) e1.E.ant.SH.gm;
	e1.E.ant.SH.up <- t :: (e1.E.ant.SH.up);
	for i = 0 to L.length e1.E.suc - 1 do
	  (L.nth e1.E.suc i).SH.gm <- L.filter (isNotThePto t) (L.nth e1.E.suc i).SH.gm;
	done;
  in
  if roots = [] then failwith "No Common Roots"
  else
	for i = 0 to L.length roots - 1 do
	  removeThePto (L.nth roots i);
	done;
  E.nf e1
;;

(* Match-main *)
let ccMatch e =
  let vv = E.fv e in
  let e0 = E.alpha_conv vv e in
  let e1 = elimLeftEq e0 in	(* Match-1 *)
  let e2 = matchPto e1 in (* Match-2 *)
  let e3 = instEx e2 in (* Match-3 *)
  let e4 = elimIncons e3 in (* Match-4 *)
  let e5 = reduceByRightEq e4 in (* Match-5 *)
  let e6 = reduceByRightNeq e5 in (* Match-6 *)
  let e7 = elimRightAllocatedEmp e6 in (* Match-7 *)
  let e8 = elimUnleaf e7 in (* Match-8 *)
  let e9 = removePto e8 in (* Match-9 *)
  e9.E.name <- e9.E.name ^ "m";
  e9
;;


(*-----------------------------------*)
(* Unfold(J)                         *)
(*-----------------------------------*)
let addElemNth n x lls =
  (* addElemNth [[x];[y]] 1 z returns [[x];[z;y]] *)
  let rec aux res ll i =
	match i,ll with
	| _,[] -> Lt.append_rev res []
	| 0,l::ll' -> Lt.append_rev res ((x::l)::ll')
	| _,l::ll' -> aux (l::res) ll' (i-1)
  in
  aux [] lls n
;;

let addElemNthL lll n x = L.map (addElemNth x n) lll;;

let decompList ls n =
  (* decompList [x;y] 2 returns [[xy,[]],[x,y],[y,x],[[],xy]] *)
  let init = L.map (fun _ -> []) (genLst n) in
  let rec f res ls =
	match ls with
	| [] -> res
	| x::xl ->
	   let resr = ref [] in
	   for i = 0 to n - 1 do
		 resr := (L.map (addElemNth i x) res) @ !resr;
	   done;
	   f !resr xl
  in
  f [init] ls
;;

let distribDown h xx =
  let xx' = setminus xx (SH.getRoots h) in (* remove trivial cell *)
  let xxxx = decompList xx' (L.length h.SH.gm) in
  let hh = ref [] in
  for i = 0 to L.length xxxx - 1 do
	let h1 = SH.clone h in
	let yyy = L.nth xxxx i in
	h1.SH.gm <- L.map (fun ((g,zz),yy)->(g,L.sort T.compare (zz@yy))) (zipLst h1.SH.gm yyy);
	hh := h1 :: !hh;
  done;
  !hh
;;

let substSH h_base pos prm h =
  (* subst the pos-th spat of h_base with lambda prm.h *)
  (* actually h is assumed to be a definition clause *)
  let vv = SH.av h_base in
  let h1 = SH.alpha_conv vv h in
  let hb1 = SH.clone h_base in
  let res = ref [] in
  begin
	match L.nth hb1.SH.gm pos with
	| (S.Emp,_) | (S.Pto(_,_),_) -> res := [hb1]
	| (S.Pr(_,uu),xx) ->
	   if L.length prm <> L.length uu
	   then failwith "substSH: parameter length mismatch"
	   else
		 let sub = zipLst prm uu in
		 let h2 = SH.subst sub h1 in
		 hb1.SH.ex <- hb1.SH.ex @ h2.SH.ex;
		 hb1.SH.out <- hb1.SH.out @ h2.SH.out;
		 hb1.SH.up <- hb1.SH.up @ h2.SH.up;
		 hb1.SH.pi <- hb1.SH.pi @ h2.SH.pi;
		 let hh = distribDown h2 xx in
		 for i = 0 to L.length hh - 1 do
		   let hb2 = SH.clone hb1 in
		   hb2.SH.gm <- Lt.replaceNth hb2.SH.gm pos (L.nth hh i).SH.gm;
		   res := hb2 :: !res
		 done;
  end;
  !res
;;

let lookupRootSH h root =
  let res = ref None in
  for i = 0 to L.length h.SH.gm - 1 do
	match L.nth h.SH.gm i with
	| (S.Emp,_) -> ()
	| (S.Pto(t,_),_) ->
	   if root = t then res := Some (i,"") else ()
	| (S.Pr(_,[]),_) -> ()
	| (S.Pr(pr,tt),_) ->
	   if root = L.hd tt then res := Some (i,pr) else ()
  done;
  !res
;;

let rec lookupIndDef pr dd =
  match dd with
  | [] -> None
  | (p,prm,df)::dd1 ->
	 if p = pr then Some (prm,df) else lookupIndDef pr dd1 
;;  

let unfoldL (dd : IS.t) e root =
  (* (Unfold-L) unfolding lhs *)
  let e1 = E.clone e in
  let res = ref [] in
  begin
  match lookupRootSH e.E.ant root with
  | None -> ()
  | Some (pos,pr) ->
	 if pr = "" then res := [e1] else
	   match lookupIndDef pr dd with
	   | None -> failwith "unfoldL: undefined inductive predicate"
	   | Some(prm,df) ->
		  for i = 0 to L.length df - 1 do
			let hh = substSH e1.E.ant pos prm (L.nth df i) in
			for j = 0 to L.length hh - 1 do
			  let e2 = E.clone e in
			  e2.E.ant <- L.nth hh j;
			  e2.E.ant.SH.ex <- [];
              let e3 = reduceByRightEq e2 in
              let e4 = elimDuplPure e3 in
              res := e4 :: !res
              (*
              res := (elimDuplPure (reduceByRightEq e2)) :: !res
               *)
			done;
		  done;
  end;
  !res
;;

let unfoldR (dd : IS.t) e root =
  (* (Unfold-R) unfolding rhs *)
  let e1 = E.clone e in
  let res = ref [] in
  for i = 0 to L.length e.E.suc - 1 do
	let h = L.nth e.E.suc i in
	match lookupRootSH h root with
	| None -> failwith "unfoldR: common root is missing in rhs"
	| Some (pos,pr) ->
	   if pr = "" then res := h :: !res else
		 match lookupIndDef pr dd with
		 | None -> failwith ("unfoldR: undefined inductive predicate" ^ pr)
		 | Some(prm,df) ->
			for i = 0 to L.length df - 1 do
			  let hh = substSH h pos prm (L.nth df i) in
			  res := hh @ !res;
			done;
  done;
  e1.E.suc <- L.rev !res;
  e1

(* Unfold-main *)
let ccUnfold dd e =
  match findCommonAllRoots e with
  | [] -> failwith "unfold: no common root"
  | root::_ ->
	 let ee = unfoldL dd (unfoldR dd e root) root in
	 for i = 0 to L.length ee - 1 do
	   let e' = L.nth ee i in
	   e'.E.name <- e.E.name ^ "u" ^ (string_of_int i);
	 done;
	 ee

let reduceContradictPreds contraList e =
  let isContraS s =
    match s with
    | S.Pr(pr,_) when L.mem pr contraList -> true
    | _ -> false
  in
  let isContraG g = isContraS (fst g) in
  let isContraGG gg = L.exists isContraG gg in
  let isContraSH h = isContraGG h.SH.gm in
  let e1 = E.clone e in
  e1.E.suc <- L.filter (fun h -> not(isContraSH h)) e1.E.suc;
  e1
;;

(*---------------------------------------*)
(* Matching                              *)
(*---------------------------------------*)
exception MatchFail
module Matcher = struct
(* matcher m for A to B is a subtitution *)
(* such that B = m(A)                    *)
  type t = (string * T.t) list

  let to_string (mm : t) =
    let tstring (s,t) = "(" ^ s ^ ":>" ^ (T.to_string t) ^ ")" in
    string_of_list tstring "," mm

  let println mm = print_endline (to_string mm)
    
  let domain mm = L.map fst mm

  let domainT mm = L.map var (domain mm)
	
  let range mm = L.map snd mm

  let domainrangeT mm = dropRed ((domainT mm) @ (range mm))
               
  let restrict b vv mm =
	(* It restricts mm on 'vv' if b is true, on 'not vv' otherwise *)
	let cond (v,_) = if b then L.mem v vv else not(L.mem v vv) in
	L.filter cond mm

  let merge (mm0 : t) (mm1 : t) : t =
	(* It merges mm0 and mm1 if they can be merged consistently *)
	(* Otherwise it fails *)
	let _mm = ref mm1 in
	begin
	  try
		for i = 0 to L.length mm0 - 1 do
		  let m = L.nth mm0 i in
		  if L.mem m !_mm then ()
		  else
			if L.mem (fst m) (L.map fst !_mm) then raise Break
			else _mm := m::!_mm			
		done;
	  with
		Break -> raise MatchFail
	end;
	!_mm

  let mergeL mmm0 mmm1 : t list =
	let _ans = ref [] in
	for i = 0 to L.length mmm0 - 1 do
	  for j = 0 to L.length mmm1 - 1 do
		let mm0 = L.nth mmm0 i in
		let mm1 = L.nth mmm1 j in
		try
		  _ans := (merge mm0 mm1) :: !_ans
		with
		  MatchFail -> ()
	  done;
	done;
	if !_ans = [] then raise MatchFail else !_ans

  let toList initmmm ofIt ll0 ll1 : t list =
	(* It tries to find matcher of ll0 and ll1 *)
	(* ll0 and ll1 are considered as lists *)
	let _ans = ref initmmm in
	let n = L.length ll0 in
	if n <> L.length ll1 then raise MatchFail else
	  for i = 0 to n - 1 do
		let mmm = ofIt (L.nth ll0 i) (L.nth ll1 i) in
		_ans := mergeL mmm !_ans;
	  done;
	!_ans

  let toMultiset initmmm ofIt ll0 ll1 : t list =
  (* It tries to find all matchers of ll0 and ll1 *)
  (* ll0 and ll1 are considered as multisets *)
	let _ans = ref [] in
	let _stack = ref [(ll0,ll1,initmmm)] in
	if L.length ll0 <> L.length ll1 then raise MatchFail else
	  if ll0 = [] then initmmm else
		begin
		  while !_stack <> [] do
			let (ll0a,ll1a,mmm) = L.hd !_stack in
			_stack := L.tl !_stack;
			try
			  for i = 0 to L.length ll1a - 1 do
				let x, ll0b = L.hd ll0a, L.tl ll0a in
				let (y,ll1b) = List_tailrec.takeNth ll1a i in
				let mmm' = mergeL (ofIt x y) mmm in
				if ll0b = [] then _ans := mmm' @ !_ans
				else _stack := (ll0b,ll1b,mmm') :: !_stack;
			  done;
			with
			  MatchFail -> ()
		  done;
		  if !_ans = [] then raise MatchFail else !_ans
		end
		  
  let ofAtom c0 c1 : t list =
	if c0 = c1 then [[]] else raise MatchFail

  let ofTerm t0 t1 =
	match t0,t1 with
	| T.Var v,_ -> [[(v,t1)]]
	| T.Nil,T.Nil -> [[]]
	| T.Nil,_ -> raise MatchFail

  let ofTermL mmm = toList mmm ofTerm

  let ofTermM mmm = toMultiset mmm ofTerm

  let ofPair ofFst ofSnd p1 p2 =
	let (x1,y1) = p1 in
	let (x2,y2) = p2 in
	let mmm1 = ofFst x1 x2 in
	let mmm2 = ofSnd y1 y2 in
	mergeL mmm1 mmm2

  let ofSpatExp s1 s2 =
	let mkSignature s =
	  match s with
	  | S.Emp -> ("emp",[])
	  | S.Pto(t,tt) -> ("pto",t::tt)
	  | S.Pr(pr,tt) -> (pr,tt)
	in
	let sg1 = mkSignature s1 in
	let sg2 = mkSignature s2 in
	ofPair ofAtom (ofTermL [[]]) sg1 sg2

  let ofGammaExp (g0 : G.t) (g1 : G.t) = 
	let (s0,tt0) = g0 in
	let (s1,tt1) = g1 in
	let mmm = ofSpatExp s0 s1 in
	toMultiset mmm ofTerm tt0 tt1

  let ofGamma mmm (gg0 : GG.t) (gg1 : GG.t) =
	toMultiset mmm ofGammaExp gg0 gg1

  let ofPureExp (p1 : P.t) (p2 : P.t) =
	let mkSignature p =
	  match p with
	  | (Eq,t,u) -> ("eq",[t;u])
	  | (Neq,t,u) -> ("neq",[t;u])
	in
	let sg1 = mkSignature p1 in
	let sg2 = mkSignature p2 in
	ofPair ofAtom (ofTermM [[]]) sg1 sg2

  let ofPure mmm (pp1 : PP.t) (pp2 : PP.t) =
	toMultiset mmm ofPureExp pp1 pp2

  let ofQFSH mmm k1 k2 =
	let (pp1,gg1) = k1 in
	let (pp2,gg2) = k2 in
	let mmm1 = ofGamma mmm gg1 gg2 in
	ofPure mmm1 pp1 pp2 

  let isVarBijectionOf vv tt (mm : t) =
	(* It checks mm gives a bijection (var.renaming) from vv to tt *)
	match L.length vv = L.length tt with
	| false ->
       failwith "isVarBijection: length mismatch"
	| true ->
	   let mm1 = L.filter (fun (v,_) -> L.mem v vv) mm in
	   let uu1 = range mm1 in
	   seteq tt uu1

  let isVarBijection mm = isVarBijectionOf (domain mm) (range mm) mm

  let ofSH mmm h1 h2 : t list =
	let (vv1,yy1,oo1,pp1,gg1) = SH.decomp h1 in
	let (vv2,yy2,oo2,pp2,gg2) = SH.decomp h2 in
	if L.length vv1 <> L.length vv2 then raise MatchFail else
	let mmm1 = ofGamma mmm gg1 gg2 in
	let mmm2 = ofTermM mmm1 oo1 oo2 in
	let mmm3 = ofTermM mmm2 yy1 yy2 in
	let mmm4 = L.filter (isVarBijectionOf vv1 (L.map var vv2)) mmm3 in
	L.map (restrict false vv1) mmm4

  let ofSHwoPure mmm h1 h2 : t list =
	let (vv1,yy1,oo1,_,gg1) = SH.decomp h1 in
	let (vv2,yy2,oo2,_,gg2) = SH.decomp h2 in
	if L.length vv1 <> L.length vv2 then raise MatchFail else
	let mmm1 = ofGamma mmm gg1 gg2 in
	let mmm2 = ofTermM mmm1 oo1 oo2 in
	let mmm3 = ofTermM mmm2 yy1 yy2 in
	let mmm4 = L.filter (isVarBijectionOf vv1 (L.map var vv2)) mmm3 in
	L.map (restrict false vv1) mmm4	  

  let ofEntl e1 e2 =
	let (_,h1,hh1) = Entl.decomp e1 in
	let (_,h2,hh2) = Entl.decomp e2 in
	let mmm1 = ofSH [[]] h1 h2 in
	toMultiset [[]] (ofSH mmm1) hh1 hh2
	  
  let ofEntlwoPure e1 e2 =
	let (id1,h1,hh1) = Entl.decomp e1 in
	let (id2,h2,hh2) = Entl.decomp e2 in
	let mmm1 = ofSHwoPure [[]] h1 h2 in
	toMultiset [[]] (ofSHwoPure mmm1) hh1 hh2
	  
  let findSH h1 h2 =
	try
	  let mmm = ofSH [[]] h1 h2 in Some mmm
	with
	  MatchFail -> None
	  
  let findEntl e1 e2 =
	try
	  let mmm = ofEntl e1 e2 in Some mmm
	with
	  MatchFail -> None


  let findEntlwoPure e1 e2 =
	try
	  let mmm = ofEntlwoPure e1 e2 in Some mmm
	with
	  MatchFail -> None		

end

module M = Matcher

(*---------------------------------------*)
(* Bud-Companion                         *)
(*---------------------------------------*)
let ccBud e ee =
(* It checks e is a bud whose companion is in ee *)
(* Namely, there is e' in ee such that m(e') = e *)
  let _ans = ref false in
  begin
	try
	  for i = 0 to L.length ee - 1 do
		match M.findEntl (L.nth ee i) e with
		| Some _ -> raise Break
		| None -> ()
	  done;
	with
	  Break -> _ans := true
  end;
  !_ans
;;

(* Simple Bud-Companion checking *)
(* It skips pure part checking   *)
let ccBudSimple e ee =
(* It checks e is a bud whose companion is in ee *)
(* Namely, there is e' in ee such that m(e') = e *)
  let _ans = ref false in
  begin
	try
	  for i = 0 to L.length ee - 1 do
		match M.findEntlwoPure (L.nth ee i) e with
		| Some _ -> raise Break
		| None -> ()
	  done;
	with
	  Break -> _ans := true
  end;
  !_ans
;;	
  
  
(*---------------------------------------*)
(* Normalize                             *)
(*---------------------------------------*)
let dropTrivMM mm =
  let isNotTrivM (v,t) = var v <> t in
  L.filter isNotTrivM mm
;;

let checkFreshVarDisjElim h h1 h2 =
  (* h:SH of ant. h1,h2 are SH in suc. It checks h1 is m'ble with h2 *)
  let (_,_,_,pp,gg) = SH.decomp h in
  let cond1 mm = subset (M.domainrangeT mm) (h.SH.up) in
  let cond2 mm = disjoint (M.domain mm) (GG.fv gg) in
  let cond3 mm =
	let fvT h = L.map var (SH.fv h) in
	let neqL = PP.nf (mkNeqAll (M.domainT mm) (T.Nil :: (fvT h))) in
	subset neqL pp
  in
  let cond mm = (cond1 mm) && (cond2 mm) && (cond3 mm) in
  match M.findSH h1 h2 with
  | None -> false
  | Some mmm ->
     let mmm0 = L.map dropTrivMM mmm in
     let mmm1 = L.filter M.isVarBijection mmm0 in
     match L.filter cond mmm1 with
     | [] -> false
     | _::_ -> true
;;

(* (Normalize-1) Fresh Variable Disjunct Elimination *)
let freshVarDisjElim e =
  let (_,h,hh) = Entl.decomp e in
  let _drop = ref [] in
  for i = 0 to L.length hh - 1 do
	for j = 0 to L.length hh - 1 do
	  if i = j then () else
	  let h1 = L.nth hh i in
	  let h2 = L.nth hh j in
	  match checkFreshVarDisjElim h h2 h1 with
	  | true -> _drop := h1 :: !_drop
	  | false -> ()
	done;
  done;
  let e1 = Entl.clone e in
  e1.Entl.suc <- setminus e1.Entl.suc !_drop;
  e1

(* (Normalize-2) Unnecessary Disequality Elimination *)
let unnecDiseqElim e =
  let e1 = Entl.clone e in
  let (_,h,hh) = Entl.decomp e in
  let (_,_,_,pp,gg) = SH.decomp h in
  let fvGS = dropRed ((GG.fv gg) @ (L.flatten (L.map SH.fv hh))) in
  let _pp = ref [] in
  for i = 0 to L.length pp - 1 do
	match L.nth pp i with
	| (Neq,T.Var v,_) when not(L.mem v fvGS) -> ()
	| (Neq,_,T.Var v) when not(L.mem v fvGS) -> ()
	| (_,_,_) as p -> _pp := p :: !_pp
  done;
  e1.Entl.ant.SH.pi <- L.rev !_pp;
  e1

(* (Normalize-3) Unnecessary Variable Elimination *)
let unnecVarElim e =
  let e1 = Entl.clone e in
  let (_,h,hh) = Entl.decomp e in
  let (_,up,out,_,gg) = SH.decomp h in
  let fvGS = dropRed ((GG.fv gg) @ (L.flatten (L.map SH.fv hh))) in
  let _up = ref [] in
  let _out = ref [] in
  for i = 0 to L.length up - 1 do
	match L.nth up i with
	| T.Var v when not(L.mem v fvGS) -> ()
	| _ as t -> _up := t :: !_up
  done;
  for i = 0 to L.length out - 1 do
	match L.nth out i with
	| T.Var v when not(L.mem v fvGS) -> ()
	| _ as t -> _out := t :: !_out
  done;
  e1.Entl.ant.SH.up <- L.rev !_up;
  e1.Entl.ant.SH.out <- L.rev !_out;
  e1  

(* (Normalize-4) Unnecessary Unrelated Existential Elimination *)
let unnecExOutElimSH h =
  let h1 = SH.clone h in
  let ttExOut = intersect (L.map var h.SH.ex) h.SH.out in
  let isFvGm t = L.mem t (L.map var (GG.fv h1.SH.gm)) in
  let isNeqTm t p =
	match p with
	| (Neq,t1,t2) -> if t1 = t || t2 = t then true else false
	| (_,_,_) -> false
  in
  for i = 0 to L.length ttExOut - 1 do
	let t = L.nth ttExOut i in
	if isFvGm t then () else
	  begin
		h1.SH.ex <- L.filter (fun v -> t <> var v) h1.SH.ex;
		h1.SH.out <- L.filter (fun u -> t <> u) h1.SH.out;
		h1.SH.pi <- L.filter (fun p -> not(isNeqTm t p)) h1.SH.pi;
	  end
  done;
  h1
  
let unnecExOutElim e =
  let e1 = Entl.clone e in
  let (_,h,hh) = Entl.decomp e in
  e1.Entl.suc <- L.map unnecExOutElimSH hh;
  e1
  
	
(* Normalize-main *)
let ccNormalize e =
  Opt.sayifDebug @@ "==> Normalization\n" ^ (Entl.to_string e);
  let _e = ref (E.clone e) in
  _e := freshVarDisjElim !_e; (* Normalize-1 *)
  _e := unnecDiseqElim !_e; (* Normalize-2 *)
  _e := unnecVarElim !_e; (* Normalize-3 *)
  _e := unnecExOutElim !_e; (* Normalize-4 *)
  Opt.sayifDebug @@ "Normalization result:\n" ^ (Entl.to_string !_e);
  !_e
;;

(*---------------------------------------*)
(* Factor                                *)
(* Step1: Factor Introduction            *)
(* Step2: Name Case Analysis             *)
(* Step3: Downarrow Analysis             *)
(*---------------------------------------*)
let addSplitWandHead ddOrig dwand wQ ww = 
  (* w means ("P",tt), ww is a list of w *)
  (* addSplitWandHead 2 R [S;Q;P] returns *)
  (* [([R;P],[S;Q]); ([R;Q],[S;P]); ([S;R],[Q;P]) ] *)
  let ( <: ) w1 w2 = fst w1 < fst w2 in
  let wCompare w1 w2 = if w1 = w2 then 0 else if w1 <: w2 then -1 else 1 in
  let underLim l = L.length l < dwand in
  let depQ = dep ddOrig (fst wQ) in
  let ww' = L.sort wCompare (wQ::ww) in
  let _ans = ref [ ([],[]) ] in
  let addone w1 (ww1,ww2) =
	match wQ = w1, underLim ww1, underLim ww2 with
	| true,true,_  -> [ (w1::ww1,ww2) ]
	| true,false,_  -> []
	| _,true,true   ->
       if L.mem (fst w1) depQ
       then [ (w1::ww1,ww2);(ww1,w1::ww2) ]
       else [ (w1::ww1,ww2) ]
	| _,true,false  -> [ (w1::ww1,ww2) ]
	| _,false,true  ->
       if L.mem (fst w1) depQ
       then [ (ww1,w1::ww2) ]
       else [ (ww1,ww2) ]
	| _,false,false -> []
  in
  for i = 0 to L.length ww' - 1 do
	_ans := L.flatten (L.map (addone (L.nth ww' i)) !_ans)
  done;
  !_ans
;;

let mkFactorIntroCore ddOrig dwand wQ (wd : Wand.t) : (Wand.t * Wand.t) list =
  let (ww,wP) = wd in
  let qq = addSplitWandHead ddOrig dwand wQ ww in
  L.map (fun (ww1,ww2) -> ((ww2,wQ), (ww1,wP))) qq
;;

(* Factor-1: Factor Introduction            *)
let factorIntro ddOrig ddWand dwand e pos tRoot: (string list * GG.t * T.t list) list =
  (* It introduces factor                         *)
  (* (R-*P)&<xx> becomes                          *)
  (* Ex #1( R-*Q(t,#1) * Q(t,#1)-*P &<xx>) and    *)
  (* Ex #1( Q(t,#1) * R,Q(t,#1)-*P &<xx>)         *)
  (* dd: ind.sys. of original language            *)
  (* dwand: limit of the depth of wands           *)
  (* e: entailment                                *)
  (* pos: the position of gamma in the succ. of e *)
  (* tRoot: the root to be exposed by Factor      *)
  (* It returns                                   *)
  (* [                                            *)
  (*   ( [#1],[R-*Q(t,#1);Q(t,#1)-*P],xx );       *)
  (*   ( [#1],[Q(t,#1); R,Q(t,#1)-*P],xx );       *)
  (* ]                                            *)
  let vvU = dropRed ((T.fv tRoot) @ (Entl.av e)) in (* used vars *)
  let (posSuc,posGm) = pos in
  let h = L.nth e.Entl.suc posSuc in
  let (s,xx) = L.nth h.SH.gm posGm in
  let _res = ref [] in
  begin
  match s with
  | S.Emp | S.Pto(_,_) -> _res := [ ([],[(s,[])],xx) ]
  | S.Pr(vWand,_) ->
	 let wd = Wand.fromSpatExp s in
	 let vP = fst (snd wd) in
	 let depP = dep ddOrig vP in
	 for i = 0 to L.length depP - 1 do
	   let vQ = L.nth depP i in
	   let prmLen =
		 match IS.lookupParamLen ddWand vQ with
		 | [] -> failwith ("factorIntro: no inductive predicate " ^ vQ)
		 | len::_ -> len
	   in
	   let vvF = genFreshVarL "#" vvU (prmLen - 1) in (* fresh vars *)
	   let ttF = L.map var vvF in
	   let wwL = mkFactorIntroCore ddOrig dwand (vQ,tRoot::ttF) wd in
	   _res :=
		 (L.map
			(fun (wd1,wd2) ->
			  (vvF, [(Wand.toSpatExp wd1,[]);(Wand.toSpatExp wd2,[])], xx))
			wwL)
		 @ !_res
	 done
  end;
  !_res
;;

(* Name Case Analysis Core           *)
let nameCaseAnalysisCore tt (vv,gg,xx) = 
  (* tt is assumed to be FV(J)+{nil} *)
  let gG,gF = L.nth gg 0, L.nth gg 1 in
  let vv' = L.rev vv in
  (* First clause making *)
  let mkFstClause v0 (vv0,pp0,gG0,gF0) =
	let t0 = var v0 in
	let gF1 = (fst gF0,t0::(snd gF0)) in
	let pp1 = (mkNeqAll [t0] tt) @ pp0 in
	(v0::vv0,pp1,gG0,gF1)
  in
  (* Second clause making *)
  let mkSndClause v0 (vv0,pp0,gG0,gF0) =
	let _cur = ref [] in
	for j = 0 to L.length tt - 1 do
	  let tj = L.nth tt j in
	  let gFj = G.subst [(v0,tj)] gF0 in
	  let gGj = G.subst [(v0,tj)] gG0 in
	  _cur := (vv0,pp0,gGj,gFj) :: !_cur;
	done;
	!_cur
  in
  let mkClause v0 vpgf = (mkFstClause v0 vpgf) :: (mkSndClause v0 vpgf) in
  (* Main routine *)
  let _res = ref [ ([],[],gG,gF) ] in
  for k = 0 to L.length vv' - 1 do
	let vk = L.nth vv' k in 
	_res := Lt.concat_rev (Lt.map_rev (mkClause vk) !_res)
  done;
  L.map (fun (vv,pp,gG,gF) -> (vv,pp,[gG;gF],xx)) !_res
;;

(* backup
let nameCaseAnalysisCore tt (vv,gg,xx) =
  (* tt is assumed to be FV(J)+{nil} *)
  let _cur = ref [ (vv,[],[],gg,xx) ] in
  let _res = ref [] in
  
  while !_cur <> [] do
	print_endline "I'm here4";
	let (vv0,vv1,pp,gg,xx) = L.hd !_cur in
	_cur := L.tl !_cur;
	if vv0 = [] then _res := (vv1,pp,gg,xx) :: !_res else
	  let v0 = L.hd vv0 in
	  let t0 = var v0 in
	  let vv0a = L.tl vv0 in
	  let gG,gF = L.nth gg 0, L.nth gg 1 in
  
	  (* begin first clause making *)
	  let gF1 = (fst gF,t0::(snd gF)) in
	  let gg1 = [gG;gF1] in
	  let pp1 = (mkNeqAll [t0] tt) @ pp in
	  _cur := (vv0a,v0::vv1,pp1,gg1,xx) :: !_cur;
	  (* end first clause making *)
	
	  (* begin second clause making *)
	  for j = 0 to L.length tt - 1 do
		let gF2 = G.subst [(v0,L.nth tt j)] gF in
		let gG2 = G.subst [(v0,L.nth tt j)] gG in
		let gg2 = [gG2;gF2] in
		_cur := (vv0a,vv1,pp,gg2,xx) :: !_cur;
	  done;
	  (* end second clause making *)
  done;
  !_res
;;
*)
(* Factor-2: Name Case Analysis             *)
let nameCaseAnalysis e tRoot (vv,gg,xx) =
  let vv0 = dropRed ((T.fv tRoot) @ (Entl.fv e)) in
  let tt0 = nil :: (L.map var vv0) in
  nameCaseAnalysisCore tt0 (vv,gg,xx)
;;	  

let nameCaseAnalysisL e tRoot vgxL =
  L.flatten (L.map (nameCaseAnalysis e tRoot) vgxL)
;;	
  
(* Factor-3: Downarrow Analysis             *)
let downArrowAnalysis (vv,pp,gg,xx) =
  let h = SH.create vv [] [] pp gg in
  let hh = distribDown h xx in
  let extractSH h = (h.SH.ex,h.SH.pi,h.SH.gm) in
  L.map extractSH hh
;;

let downArrowAnalysisL vpgxL =
  L.flatten (L.map downArrowAnalysis vpgxL)
;;

let ccFactor ddOrig ddWand dwand pos tRoot e =
  let e1 = E.clone e in
  let (posSuc,posGm) = pos in
  let vgxL = factorIntro ddOrig ddWand dwand e pos tRoot in (* Factor-1 *)
  let vpgxL = nameCaseAnalysisL e tRoot vgxL in (* Factor-2 *)
  let vpgL = downArrowAnalysisL vpgxL in (* Factor-3 *)
  let h = L.nth e.Entl.suc posSuc in
  let _hh = ref [] in
  for i = 0 to L.length vpgL - 1 do
	let (vv,pp,gg) = L.nth vpgL i in
	let h' = SH.clone h in
	h'.SH.ex <- vv @ h'.SH.ex;
	h'.SH.pi <- pp @ h'.SH.pi;
	h'.SH.gm <- Lt.replaceNth h'.SH.gm posGm gg;
	_hh := h' :: !_hh
  done;
  let dummy_h = SH.create [] [] [] [T.Nil <.> T.Nil] [] in
  e1.Entl.suc <- e1.Entl.suc @ !_hh;
  e1.Entl.suc <- Lt.replaceNth e1.Entl.suc posSuc [dummy_h];
  e1
;;

(* reducing by checking Right-contradiction *)
(* backup これは効率が悪い
let reducingRight ddWand e =
  let e1 = Entl.clone e in
  let pp = e1.Entl.ant.SH.pi in
  let checkRight h =
    let h1 = SH.clone h in
    h1.SH.pi <- pp @ h1.SH.pi;
    CcSatcheck.decideSatSH ddWand h1
  in
  e1.Entl.suc <- L.filter checkRight e1.Entl.suc;
  e1
;;
 *)
let reducingRight yyWand e = 
  let e1 = Entl.clone e in
  let pp = e1.Entl.ant.SH.pi in
  let checkRight h =
    let pp1 = CcIndDefAnalysis.extractFromSH yyWand h in
    SHpure.satcheck (pp @ pp1)
  in
  e1.Entl.suc <- L.filter checkRight e1.Entl.suc;
  e1
;;

(*---------------------------------------*)
(* SplitPreproc                          *)
(* 1: Add Missing Definedness + Distrib  *)
(* 2: Disjunct Grouping                  *)
(* 3: Existential Disequality            *)
(* 4: Unrelatedness Introduction         *)
(* 5: Existential Split                  *)
(* 6: MatchRemove (for efficiency)       *)
(* -> then do Split                      *)
(*---------------------------------------*)
  
(* SplitPre-1: Add Missing Definedness + Distrib  *)
(* IN:  ls(x,y)*ls(y,z) |- ls(y,Nil) | ls(x,z) *)
(* OUT: ls(x,y)*ls(y,z) |- ls(y,Nil)&<x> | ls(x,z)&<y> *)
let addMissingDefinedness (e : Entl.t) : Entl.t =
  let (id,h,hh) = Entl.decomp e in
  let ttRoots = SH.getRoots h in
  let _res = ref [] in
  for i = 0 to L.length hh - 1 do
	let h1 = L.nth hh i in
	let ttMissing = setminus ttRoots (SH.getRootCells h1) in
	_res := (distribDown h1 ttMissing) @ !_res
  done;
  { Entl.name = id; Entl.ant = h; Entl.suc = dropRed (L.rev !_res) }
;;
  
let mkVarGroup h = L.map G.getRootCells h.SH.gm
;;

let checkSameGroup varGrpL t1 t2 =
  let _res = ref false in
  let isInNthGrp t i = L.mem t (L.nth varGrpL i) in
  for i = 0 to L.length varGrpL - 1 do
	if isInNthGrp t1 i && isInNthGrp t2 i
	then _res := true
	else ()
  done;
  !_res
;;

let mkGroupingCore h1 h2 : GrpSHs.t list = 
  (* h1: the SH in the lhs of entl        *)
  (* h2: a SH in the rhs of entl          *)
  (* It makes grouping of h2, it may fail *)
  let vv = h2.SH.ex in
  let up = h2.SH.up in
  let out = h2.SH.out in
  let pp = h2.SH.pi in
  let gg = h2.SH.gm in
  let varGrpL = mkVarGroup h1 in
  let _rest = ref gg in
  let _cur = ref [] in
  let _result = ref [] in
  for i = 0 to L.length varGrpL - 1 do
	let gp = L.nth varGrpL i in
	_cur := [];
	let len = L.length !_rest in
	for j = 0 to len - 1 do
	  let j' = len - j - 1 in
	  let g = L.nth !_rest j' in
	  let xx = (L.map var vv) @ out @ up in
	  if subset (setminus (G.getRootCells g) xx) gp
	  then
		begin
		  _cur := g :: !_cur;
		  _rest := Lt.replaceNth !_rest j' []
		end
	  else ()
	done;
	let curSH = { SH.ex = []; SH.out = []; SH.up = []; SH.pi = pp; SH.gm = !_cur } in
	_result := curSH :: !_result
  done;
  if !_rest <> []
  then begin GG.println !_rest; failwith "Grouping Failure" end
  else
	[{ GrpSHs.ex = vv; GrpSHs.up = up; GrpSHs.out = out; GrpSHs.body = !_result }]
;;

let mkGrpEntl (e : Entl.t) : GrpEntl.t =
  let (id,h,hh) = Entl.decomp e in
  let kkk = L.flatten @@ L.map (mkGroupingCore h) hh in
  { GrpEntl.name = id; GrpEntl.ant = h; GrpEntl.suc = kkk }
;;

let findFactorPos e1 =
  let varGrpL = mkVarGroup e1.Entl.ant in
  let hh = e1.Entl.suc in
  let _res = ref None in
  begin
	try
	  for i = 0 to L.length hh - 1 do
		let h = L.nth hh i in
		let gg = h.SH.gm in
		for j = 0 to L.length gg - 1 do
		  match L.nth gg j with
		  | (S.Pr(_,t::_),xx) ->
			 for k = 0 to L.length xx - 1 do
			   let x = L.nth xx k in
			   if (checkSameGroup varGrpL t x) || (L.mem x (L.map var h.SH.ex))				 
			   then ()
			   else
				 begin
				   _res := Some ((i,j),x);
				   raise Break
				 end;
			 done;
		  | (_,_) -> ()
		done;
	  done;
	with Break -> ()
  end;
  !_res
;;

(* SplitPre-2: Disjunct Grouping             *)
let disjunctGrouping ddOrig ddWand uu yyWand contraIP dwand e : GrpEntl.t =
  let _e = ref (Entl.clone e) in
  begin
	try
	  while true do
		Opt.doifDebug (fun _ -> print_string "Find factor root --> ") ();
		match findFactorPos !_e with
		| None ->
		   begin
			 Opt.sayifDebug "none\n";
			 raise Break
		   end
		| Some(pos,x) ->
		   begin
			 Opt.sayifDebug @@ "Do factor for " ^ (T.to_string x);
			 _e := ccFactor ddOrig ddWand dwand pos x !_e;
			 Opt.sayifDebug @@ "Factor result:";
			 Opt.sayifDebug @@ Entl.to_string !_e;
			 (*---*)
             Opt.sayifDebug ">>> Reducing Contradict Inductive Predicates";
             _e := reduceContradictPreds contraIP !_e;
			 Opt.sayifDebug @@ Entl.to_string !_e;
			 (*---*)
             Opt.doifOpt @@ (fun _ ->
               begin
                 Opt.sayifDebug @@ ">>> Reducing by Right Contradiction checking";
                 _e := reducingRight yyWand !_e;
		         Opt.sayifDebug @@ Entl.to_string !_e;
                 (*----*)
                 Opt.sayifDebug ">>> Reducing by IndDef Analysis";
                 _e := CcFactorAnalysis.updateEntlWithUU uu !_e
               end);
		     Opt.sayifDebug @@ Entl.to_string !_e;
             (*----*)
             Opt.sayifDebug @@ ">>> Simplification of the factor result";
             Opt.doifOpt @@ (fun _ -> 
               begin
                 let yy = CcFactorAnalysis.computeYY ddWand in
                 let uu = (CcFactorAnalysis.fromYYtoUU yy) in
			     _e := CcFactorAnalysis.updateEntlWithUU uu !_e;
               end);
             _e := instEx !_e;
             !_e.Entl.suc <- L.filter (fun h -> PP.satcheck (!_e.E.ant.SH.pi @ h.SH.pi)) !_e.Entl.suc;
             !_e.Entl.suc <- L.map dropTrivEq !_e.Entl.suc;
             Opt.sayifDebug @@ Entl.to_string !_e ^ "\n";
             (*---*)
		   end
	  done
	with Break -> ()
  end;
  mkGrpEntl !_e
;;	

(* SplitPre-3: Existential Disequality            *)
let existentialDisequalityOne vvFV (kk : GrpSHs.t) =
  (* vvFV is free vars in the other disjuncts of entl *)
  let kk1 = GrpSHs.clone kk in
  let (vv,up,out,body) = GrpSHs.decomp kk1 in
  let ttFV = L.map var vvFV in
  for i = 0 to L.length body - 1 do
	let h = L.nth body i in
	let pp = h.SH.pi in
	let ttEx = L.map var (h.SH.ex @ vv) in
	let ppNeq = mkNeqAll ttEx (nil :: ttFV) in
	let _ppCur = ref pp in
	for j = 0 to L.length ppNeq - 1 do
	  let (op,t,u) = L.nth ppNeq j in
	  if L.mem (op,t,u) pp || L.mem (op,u,t) pp then ()
	  else _ppCur := (op,t,u) :: !_ppCur;
	done;
	h.SH.pi <- !_ppCur
  done;
  kk1
;;

let existentialDisequality (e : GrpEntl.t) =
  let (id,h,kkk) = GrpEntl.decomp e in
  let vvFV = dropRed (L.sort compare (L.flatten (L.map GrpSHs.fv kkk))) in
  let kkk1 = L.map (existentialDisequalityOne vvFV) kkk in
  { GrpEntl.name = id; GrpEntl.ant = h; GrpEntl.suc = kkk1 }
;;	

(* 4: Unrelatedness Introduction         *)
let unrelatednessIntroOne (kk : GrpSHs.t) =
  let kk1 = GrpSHs.clone kk in
  let hh = kk1.GrpSHs.body in
  for i = 0 to L.length hh - 1 do
	let hi = L.nth hh i in
	let tt = SH.getCells hi in
	for j = 0 to L.length hh - 1 do
	  if i = j then () else
		let hj = L.nth hh j in
		hj.SH.out <- dropRed (L.sort T.compare (tt @ hj.SH.out))
	done;
  done;
  kk1
;;

let unrelatednessIntro (ge : GrpEntl.t) =
  let ge1 = GrpEntl.clone ge in
  let kkk = ge1.GrpEntl.suc in
  ge1.GrpEntl.suc <- L.map unrelatednessIntroOne kkk;
  ge1
;;

(* 5: Existential Split                  *)
let existentialSplitOne (kk : GrpSHs.t) =
  let kk1 = GrpSHs.clone kk in
  let (vv,up,out,body) = GrpSHs.decomp kk1 in
  for i = 0 to L.length body - 1 do
	let hi = L.nth body i in
	hi.SH.ex <- dropRed (L.sort compare (vv @ hi.SH.ex));
	hi.SH.up <- dropRed (L.sort compare (up @ hi.SH.up));
	hi.SH.out <- dropRed (L.sort compare (up @ hi.SH.out))
  done;
  kk1.GrpSHs.ex <- [];
  kk1.GrpSHs.up <- [];
  kk1.GrpSHs.out <- [];
  kk1
;;

let existentialSplit (ge : GrpEntl.t) =
  let ge1 = GrpEntl.clone ge in
  let kkk = ge1.GrpEntl.suc in
  ge1.GrpEntl.suc <- L.map existentialSplitOne kkk;
  ge1
;;    

(* 6: MatchRemove                 *)
(* ge ¤Îº¸ÊÕ¤Îgg¤Ë(s,[])¤¬¤¢¤ê¡¤±¦ÊÕ¤Îgg¤Ë¶¦ÄÌ¤·¤Æ(s,[])¤¬¤¢¤ë¤È¤­¡¤Î¾ÊÕ¤«¤é¼è¤ê½ü¤¯ *)
(* º¸ÊÕ¤Îup ¤Ë s ¤Îroot¤òÄÉ²Ã¤¹¤ë *)
let matchRemoveSH (ge : GrpEntl.t) =
  let removeCondSH g h = h.SH.ex = [] &&  h.SH.gm = [g] in
  let removeCondKK g kk = L.exists (removeCondSH g) kk.GrpSHs.body in
  let removeSH g kk =
    let kk1 = GrpSHs.clone kk in
    kk1.GrpSHs.body <- L.filter (fun h -> not(removeCondSH g h)) kk1.GrpSHs.body;
    kk1
  in
  let ge1 = GrpEntl.clone ge in
  let h = ge.GrpEntl.ant in
  let kkk = ge.GrpEntl.suc in
  let gg = h.SH.gm in
  let _ggant = ref h.SH.gm in
  let _gg = ref gg in
  let _kkk = ref kkk in
  let _ttRootCells = ref (ge1.GrpEntl.ant.SH.up) in
  for i = 0 to L.length gg - 1 do
    let g = L.nth gg i in
    if L.for_all (removeCondKK g) kkk
    then
      begin
        _ttRootCells := (G.getRootCells g) @ !_ttRootCells;
        _ggant := setminus !_ggant [g];
        _kkk := L.map (removeSH g) !_kkk
      end
    else 
      ()
  done;
  ge1.GrpEntl.ant.SH.gm <- !_ggant;
  ge1.GrpEntl.suc <- !_kkk;
  ge1.GrpEntl.ant.SH.up <- !_ttRootCells;
  ge1
;;

let ccSplitPreproc ddOrig ddWand uu yyWand contraIP dwand (e : Entl.t) =
  (* Example Input: ls(x,y)*ls(y,z) |- ls(x,y)*ls(y,z) | ls(y,Nil) | ls(x,z) *)
  
  (* SplitPre-1: Add Missing Definedness + Distrib  *)
  (* ls(x,y)*ls(y,z) |- ls(x,y)*ls(y,z) | ls(y,Nil)&<x> | ls(x,z)&<y> *)

  Opt.sayifDebug @@ "Target:";
  Opt.sayifDebug @@ Entl.to_string e;

  Opt.sayifDebug @@ "===> AddMissingDefinedness";
  let e0 = addMissingDefinedness e in
  let e1 = reduceByCheckingGroup e0 in
  e1.Entl.name <- e.Entl.name ^ "g-1";
  Opt.sayifDebug @@ Entl.to_string e1;
  
  (* SplitPre-2: Disjunct Grouping *)
  Opt.sayifDebug @@ "===> DisjunctGrouping";
  let ge2 = disjunctGrouping ddOrig ddWand uu yyWand contraIP dwand e1 in
  ge2.GrpEntl.name <- e.Entl.name ^ "g-2";
  Opt.sayifDebug @@ GrpEntl.to_string ge2;
  
  (* SplitPre-3: Existential Disequality *)
  Opt.sayifDebug @@ "===> ExistentialDisequality";
  let ge3 = existentialDisequality ge2 in
  ge3.GrpEntl.name <- e.Entl.name ^ "g-3";
  Opt.sayifDebug @@ GrpEntl.to_string ge3;
  
  (* 4: Unrelatedness Introduction *)
  Opt.sayifDebug @@ "===> Unrelatedness Introduction";
  let ge4 = unrelatednessIntro ge3 in
  ge4.GrpEntl.name <- e.Entl.name ^ "g-4";
  Opt.sayifDebug @@ GrpEntl.to_string ge4;
  
  (* 5: Existential Split *)
  Opt.sayifDebug @@ "===> Existential Split";
  let ge5 = existentialSplit ge4 in
  ge5.GrpEntl.name <- e.Entl.name ^ "g-5";
  Opt.sayifDebug @@ GrpEntl.to_string ge5;

  (* 6: MatchRemove *)
  Opt.sayifDebug @@ "===> Match-Remove";
  let ge6 = matchRemoveSH ge5 in
  ge6.GrpEntl.name <- e.Entl.name ^ "g";
  Opt.sayifDebug @@ GrpEntl.to_string ge6;  
  
  ge6
;;	
