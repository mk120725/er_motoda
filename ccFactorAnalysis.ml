open Tools
open CcSyntax

module Yvec = struct
  (* [(P,[%0;%1],[ppSC1]); (Q,[%0],[ppSC2])] *)
  type t = (string * string list * PP.t list) list

  let rec lookup (yy : t) p =
	match yy with
	| [] -> None
	| (q,prm,pp) :: yy1 when p = q -> Some(prm,pp)
	| (_,_,_) :: yy1 -> lookup yy1 p
                      
end

let dropUnObservable vv pp : PP.t =
  let rec aux res1 pp1 =
	match pp1 with
	| [] -> L.rev res1
	| (Eq,T.Var v,t) :: pp2 when L.mem v vv ->
	   let res3 = PP.subst [(v,t)] res1 in
	   let pp3 = PP.subst [(v,t)] pp2 in
	   aux res3 pp3
	| (Eq,t,T.Var v) :: pp2 when L.mem v vv ->
	   let res3 = PP.subst [(v,t)] res1 in
	   let pp3 = PP.subst [(v,t)] pp2 in
	   aux res3 pp3
	| p :: pp2 -> aux (p::res1) pp2
  in
  let isObservable (_,t,u) =
	let vv1 = (T.fv t) @ (T.fv u) in
	intersect vv1 vv = []
  in
  let ppA = aux [] (PP.nf pp) in
  L.filter isObservable ppA
;;

let extractPPfromS yy s =
  match s with
  | S.Emp -> [[]]
  | S.Pto(_,_) -> [[]]
  | S.Pr(p,tt) ->
	 match Yvec.lookup yy p with
	 | None -> []
	 | Some(prm,ppp) ->
		let sub = zipLst prm tt in
		L.map (PP.subst sub) ppp
;;

let extractPPfromG yy (g : G.t) = extractPPfromS yy (fst g)
;;

let extractPPfromGG yy (gg : GG.t) =
  let pppp = (L.map (extractPPfromG yy) gg) in
  L.map L.flatten (List_tailrec.allChoice pppp)
;;

let extractPPfromSH yy (h : SH.t) : PP.t list =
  let vv = h.SH.ex in
  let pp = h.SH.pi in
  let ppp = extractPPfromGG yy h.SH.gm in
  L.map (fun pp1 -> PP.nf (dropUnObservable vv (pp@pp1))) ppp
;;

let extractPPfromSHL yy hh : PP.t list =
  dropRed (L.flatten @@ L.map (extractPPfromSH yy) hh)
;;

let updateYY1 yy (p,prm,hh) =
  let ppp = extractPPfromSHL yy hh in
  (p,prm,ppp)
;;

let updateYY (dd : IndSys.t) yy : Yvec.t = L.map (updateYY1 yy) dd
;;

let computeYY dd : Yvec.t =
  let yyInit = L.map (fun (p,prm,_) -> (p,prm,[])) dd in
  let _yy = ref yyInit in
  let _yyNext = ref (updateYY dd yyInit) in
  while !_yy <> !_yyNext do
	_yy := !_yyNext;
	_yyNext := updateYY dd !_yyNext;
  done;
  !_yy
;;

let fromYYtoUU (yy : Yvec.t) : Yvec.t = L.map (fun (p,prm,ppp) -> (p,prm,[intersectL ppp])) yy
;;

(* isSatWithUU: main function of this file                    
(* uu contains requirements generated from ddWand             *)
(* pp is an assumption                                        *)
(* isSatWithUU uu pp h checks whether h satisfies uu under pp *)
let isSatWithUU uu pp h =
  let pp1 = L.hd (extractPPfromGG uu h.SH.gm) in
  (*
  let vvFV = PP.fv pp in
  let ttFV = L.map var vvFV in
  let ppNeq = CcSyntax.mkNeqAll1 ttFV in
   *)
  PP.satcheck (pp @ pp1)
;;
*)
  
let updateSHWithUU uu h =
  let pp1 = L.hd (extractPPfromGG uu h.SH.gm) in
  let h1 = SH.clone h in
  h1.SH.pi <- pp1 @ h.SH.pi;
  h1
;;

(* The main function of this file                    *)
(* It updates an entailment with uu                  *)
(* uu contains requirements generated from ddWand    *)
let updateEntlWithUU uu e =
  let e1 = Entl.clone e in
  e1.Entl.suc <- L.map (updateSHWithUU uu) e1.Entl.suc;
  e1
;;

