open Tools;;
open CcSyntax;;

module SLX = SlxSyntax;;
module CC = CcSyntax;;
  
exception UnsatFound of string
;;

(* split *)
(* It is used for making converted inductive definitions *)
let addOneSomewhere x ll =
  let i = ref 0 in
  let res = ref [] in
  let n = List.length ll in
  let numLst = List.rev (genLst n) in
  let zz = zipLst ll numLst in
  let update ll =
	let tt = List.map (fun(t,j)->if !i = j then (x::t,j) else (t,j)) ll in
	List.map fst tt
  in
  while !i < n do
	res := (update zz) :: !res;
	i := !i+1;
  done;
  !res
;;

let mkSplit n ll =
  let init = ref [] in
  let i = ref 0 in
  while !i <  n do
	init := [] :: !init;
	i := !i+1;
  done;
  let res = ref [!init] in
  List.iter
	(fun x ->
	  res := List.flatten (List.map (addOneSomewhere x) !res)
	)
	ll;
  !res
;;

let mkEqPairs l =
  let rec aux res l = 
	match l with
	| [] -> res
	| [t] -> res
	| t::(u::_ as r) -> aux ((t,u)::res) r
  in
  aux [] l
;;

(* (P(t,u) [2;1] ->  (Pure,P@2(t,u,x#2,x#1)) *)
let convIndDefG (g : G.t) nn : PP.t * G.t =
  match g with
  | (S.Emp,_) -> ([],(S.Emp,[]))		(* dummy *)
  | (S.Pto(t,tt),_) ->
	 let vv = L.map (fun n -> "#" ^ (string_of_int n)) nn in
	 let xx = L.map var vv in
	 let pp = L.map (fun (t,u) -> t =.= u) (mkEqPairs (t::xx)) in
	 (pp,(S.Pto(t,tt),[]))
  | (S.Pr(pr,tt),_) ->
	 let n = L.length nn in
	 let pr1 = pr^"@"^(string_of_int n) in
	 let tt1 = L.map (fun i -> var ("#" ^ (string_of_int i))) nn in
	 let tt2 = tt @ tt1 in
	 ([],(S.Pr(pr1,tt2),[]))
;;

(* [P(t)+[];Q(u)+[]] [[2;1];[1]] -> [P(t,x#2,x#1)+[];Q(u,x#1)+[]] *)
let convIndDefGG gg nnn : PP.t * GG.t =
  let n = L.length gg in
  let i = ref 0 in
  let pp1 = ref [] in
  let gg1 = ref [] in
  while !i < n do
	let (pp,g) = convIndDefG (L.nth gg !i) (L.nth nnn !i) in
	pp1 := pp @ !pp1;
	gg1 := g :: !gg1;
	i := !i + 1;
  done;
  (L.rev !pp1,L.rev !gg1)
;;  

(* (ex,up,out,pp,gg) nn -> (ex,up,out,pp@pp1,gg1) *)
let convIndDefH1 h nnn =
  let (pp,gg) = convIndDefGG h.SH.gm nnn in
  (pp @ h.SH.pi,gg)
;;

let convIndDefH nn h =
  let k = L.length h.SH.gm in
  let nnnn = mkSplit k nn in
  let seed = L.map (convIndDefH1 h) nnnn in
  let res = ref [] in
  let i = ref 0 in
  while !i < L.length seed do
	let (pp,gg) = L.nth seed !i in
	let h1 = SH.create h.SH.ex h.SH.up h.SH.out pp gg in
	i := !i + 1;
	res := h1 :: !res;
  done;
  !res
;;

let convIndDef n d : IndDef.t =
  let (pr,prm,hh) = d in
  let pr1 = pr ^ "@" ^ (string_of_int n) in
  let nn = L.rev (genLst n) in
  let prm1 = L.map (fun i -> "#" ^ (string_of_int i)) nn in
  let prm2 = prm @ prm1 in
  let hh1 = L.flatten (L.map (convIndDefH nn) hh) in
  (pr1,prm2,hh1)
;;				  

let convIndSys m (dd : IS.t) : IS.t =
  let res = ref [] in
  let i = ref 0 in
  while !i <= m do
	res := !res @ (L.map (convIndDef !i) dd);
	i := !i + 1;
  done;
  !res
;;

let convSHG (g : G.t) : PP.t * G.t =
  let (s,tt) = g in
  let n = L.length tt in
  match s with
  | S.Emp as q ->
     if n = 0 then ([],(q,[]))
     else raise (UnsatFound "Emp cannot appear with Allocated Vars.")
  | S.Pto(u,uu) as q ->
	 let pp = L.map (fun (x,y) -> x =.= y) (mkEqPairs (u::tt)) in
	 (pp,(q,[]))
  | S.Pr(pr,uu) ->
	 let pr1 = pr ^ "@" ^ (string_of_int n) in
	 ([],(S.Pr(pr1,uu@tt),[]))
;;

let convSHGG (gg : GG.t) =
  let n = L.length gg in
  let i = ref 0 in
  let pp1 = ref [] in
  let gg1 = ref [] in
  while !i < n do
	let (pp,g) = convSHG (L.nth gg !i) in
	pp1 := pp @ !pp1;
	gg1 := g :: !gg1;
	i := !i + 1;
  done;
  (L.rev !pp1,L.rev !gg1)  
;;

let convSH h =
  let gg = h.SH.gm in
  let pp = h.SH.pi in
  let (pp1,gg1) = convSHGG gg in
  SH.create h.SH.ex h.SH.up h.SH.out (pp @ pp1) gg1
;;

let getMaxDown h =
  let gg = h.SH.gm in
  let nn =  L.map (fun g -> L.length (snd g)) gg in
  let i = ref 0 in
  let mx = ref 0 in
  while !i < L.length nn do
	if !mx < L.nth nn !i then mx := L.nth nn !i else ();
	i := !i + 1;
  done;
  !mx
;;

let convPS (ps : PS.t) : PS.t =
  let (e,dd) = ps in
  let h = e.Entl.ant in
  let h1 = convSH h in
  let mx = getMaxDown h in
  let dd1 = convIndSys mx dd in
  let e1 = { Entl.name=e.Entl.name; Entl.ant=h1; Entl.suc=[] } in
  (e1,dd1)
;;  

module FromCCtoSLX = struct
  (*------------------------------*)
  (* Interpretation from CC to SL *)
  (*------------------------------*)
  let of_T t =
	match t with
	| T.Nil -> SLX.SHterm.Nil
	| T.Var v -> SLX.SHterm.Var v

  let of_P (p : P.t) =
	let (op,t1,t2) = p in
	let u1 = of_T t1 in
	let u2 = of_T t2 in
	match op with
	| Eq -> SLX.SHpureExp.Eq(u1,u2)
	| Neq -> SLX.SHpureExp.Neq(u1,u2)


  let of_PP (pp : PP.t) = L.map of_P pp

  let of_S (s : S.t) =
	match s with
	| S.Emp -> SLX.SHspatExp.Emp
	| S.Pto(t,tt) ->
	   let u = of_T t in
	   let uu = L.map of_T tt in
	   SLX.SHspatExp.Alloc(u,uu)
	| S.Pr(pr,tt) ->
	   let uu = L.map of_T tt in
	   SLX.SHspatExp.Ind(pr,uu)


  let of_G (g : G.t) = of_S (fst g)
	
  let of_GG (gg : GG.t) = L.map of_G gg

  let of_SH (h : SH.t) : SLX.SH.t =
	(* Some fields are omitted but it's no problem,       *)
	(* since this function is used only for SH of ind.def *)
	let qq = of_PP (h.SH.pi) in
	let ww = of_GG (h.SH.gm) in
	([],qq,ww)

  let of_ID d = 
	let (pr,prm,hh) = d in
	let kk = L.map of_SH hh in
	(pr,prm,kk)

  let of_IS (dd : IS.t) : SLX.IS.t =
	L.map of_ID dd

end
;;

module FromSLXtoCC = struct
  (*------------------------------*)
  (* Interpretation from SL to CC *)
  (*------------------------------*)
	
  let of_T t =
	match t with
	| SLX.SHterm.Nil -> T.Nil
	| SLX.SHterm.Var v -> T.Var v

  let of_P p : P.t =
	match p with
	| SLX.SHpureExp.Eq(u1,u2) ->
	   let t1 = of_T u1 in
	   let t2 = of_T u2 in
	   (Eq,t1,t2)
	| SLX.SHpureExp.Neq(u1,u2) ->
	   let t1 = of_T u1 in
	   let t2 = of_T u2 in
	   (Neq,t1,t2)

  let of_PP pp : PP.t = L.map of_P pp

  let of_S s =
	match s with
	| SLX.SHspatExp.Emp -> S.Emp
	| SLX.SHspatExp.Alloc(t,tt) ->
	   let u = of_T t in
	   let uu = L.map of_T tt in
	   S.Pto(u,uu)
	| SLX.SHspatExp.Ind(pr,tt) ->
	   let uu = L.map of_T tt in
	   S.Pr(pr,uu)
	   
  let of_SS ss = L.map of_S ss
		 
  let of_SH shx =
	let (vv,pp,ss) = shx in
	let tt' = L.map var vv in
	let pp' = of_PP pp in
	let ss' = of_SS ss in
	let gg' = L.map (fun s -> (s,[])) ss' in
	{SH.ex=[]; SH.up=tt'; SH.out=[]; SH.pi=pp'; SH.gm=gg'}

end
;;

let decideSat (dd : IS.t) (h : SH.t) =
  let dd' = FromCCtoSLX.of_IS dd in
  let (_,pp',ss') = FromCCtoSLX.of_SH h in
  let out' = L.map FromCCtoSLX.of_T (h.SH.out @ h.SH.up) in
  if L.mem SLX.SHterm.Nil out'
  then (None,"")
  else
	let shx' = (SLX.SHterm.extractVars out',pp',ss') in
	match SlxSatcheck.decideSat dd' shx' with
	| (None,record) -> (None,record)
	| (Some(vv,pp,modelOpt),record) ->
	   let pp' = FromSLXtoCC.of_PP pp in
	   match modelOpt with
	   | None -> (Some(vv,pp',None),record)
	   | Some(shx) ->
		  let sh_model = FromSLXtoCC.of_SH shx in
		  (Some(vv,pp',Some(sh_model)),record)
;;

let printBP (vv,pp) = 
  print_endline "Found BasePair:";
  print_endline @@ "V:[" ^ (concatStrLComma vv) ^ "]";
  print_endline @@ "P:[" ^ (SHpure.to_string pp) ^ "]\n"
;;

let decideSatMain (ps : PS.t) =
  let ps' = PS.nf ps in
  try
    let ps1 = convPS ps' in
    let (e,dd) = ps1 in
    match decideSat dd e.Entl.ant with
	| (None,_) -> false
	| (Some(vv,pp,None),_) -> true
	| (Some(vv,pp,Some(sh_model)),_) -> true
  with
    UnsatFound mes -> false
;;

let decideSatSH dd h =
  let e = Entl.create "" h [] in
  decideSatMain (e,dd)
