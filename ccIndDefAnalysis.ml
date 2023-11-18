open Tools
open CcSyntax
   
module BaseData = struct
  (* A basedata is (pp,allc) *)
  (* pp is pure formula                    *)
  (* allc is allocated terms               *)
  type t = PP.t * T.t list

  let to_string (b : t) =
    let (pp,tt) = b in
    let ppStr = "pi=[" ^ (PP.to_string pp) ^ "]" in
    let ttStr = "alloc=[" ^ (string_of_list T.to_string "," tt) ^ "]" in
    "(" ^ ppStr ^ "," ^ ttStr ^ ")"

  let println (b : t) = print_endline (to_string b)
    
  let subst sub b =
    let (pp,tt) = b in
    let pp1 = PP.subst sub pp in
    let tt1 = L.map (T.subst sub) tt in
    (pp1,tt1)

  let merge (bb : t list) : t =
    let pp = dropRed @@ L.flatten (L.map fst bb) in
    let tt = dropRed @@ L.flatten (L.map snd bb) in
    (pp,tt)

  let dropUnObservable vv (b : t) : t =
  (* vv is a list of unobservable vars *)
  let rec aux resPP resAllc pp1 =
	match pp1 with
	| [] -> (L.rev resPP,resAllc)
	| (Eq,T.Var v,t) :: pp2 when L.mem v vv ->
	   let resPP' = PP.subst [(v,t)] resPP in
       let resAllc' = L.map (T.subst [(v,t)]) resAllc in
	   let pp2' = PP.subst [(v,t)] pp2 in
	   aux resPP' resAllc' pp2'
	| (Eq,t,T.Var v) :: pp2 when L.mem v vv ->
	   let resPP' = PP.subst [(v,t)] resPP in
       let resAllc' = L.map (T.subst [(v,t)]) resAllc in
	   let pp2' = PP.subst [(v,t)] pp2 in
	   aux resPP' resAllc' pp2'
	| p :: pp2 -> aux (p::resPP) resAllc pp2
  in
  let isObservablePP (_,t,u) =
	let vv1 = (T.fv t) @ (T.fv u) in
	intersect vv1 vv = []
  in
  let isObservableAllc t = not (L.mem t (L.map var vv)) in
  let (ppA,ttA) = aux [] (snd b) (fst b) in
  let ppA1 = dropRed @@ L.filter isObservablePP ppA in
  let ttA1 = dropRed @@ L.filter isObservableAllc ttA in
  (ppA1,ttA1)

  let addPossibleAlloc (b : t) : t =
    let (pp,tt) = b in
    let rec aux ttRes pp0 =
      match pp0 with
      | [] -> L.sort T.compare (dropRed ttRes)
      | (Eq,t,u) :: pp1 when L.mem t tt -> aux (u::ttRes) pp1
      | (Eq,t,u) :: pp1 when L.mem u tt -> aux (t::ttRes) pp1
      | (_,_,_) :: pp1 -> aux ttRes pp1
    in
    let tt1 = aux tt pp in
    (pp,tt1)
  
end

module YY = struct                
  (* (pr,prm,bb) *)
  type t = (string * string list * BaseData.t list) list

  let to_string (yy : t) =
    let to_string1 (pr,prm,bb) =
      let bbStr = string_of_list BaseData.to_string "; " bb in
      let prmStr = "[" ^ (string_of_list (fun x -> x) "," prm) ^ "]" in
      "(" ^ pr ^ "," ^ prmStr ^ "," ^ bbStr ^ ")"
    in
    string_of_list to_string1 "\n" yy

  let println (yy : t) = print_endline (to_string yy)

  let rec lookup (yy : t) pr =
	match yy with
	| [] -> None
	| (pr1,prm,bb) :: yy1 when pr = pr1 -> Some (prm,bb)
	| (_,_,_) :: yy1 -> lookup yy1 pr

  let extractFromS (yy : t) s : BaseData.t list =
    match s with
    | S.Emp -> [ ([],[]) ]
    | S.Pto(t,_) -> [ ([],[t]) ]
    | S.Pr(pr,tt) ->
	   match lookup yy pr with
	   | None -> [ ([],[]) ]
	   | Some(prm,bb) ->
		  let sub = zipLst prm tt in
		  L.map (BaseData.subst sub) bb

  let extractFromG yy (g : G.t) : BaseData.t list =
    let bb = extractFromS yy (fst g) in
    L.map (fun (pp,tt) -> (pp,tt@(snd g))) bb

  let extractFromGG yy (gg : GG.t) =
    let bbb = (L.map (extractFromG yy) gg) in
    L.map BaseData.merge (List_tailrec.allChoice bbb)

  let extractFromSH yy (h : SH.t) = 
    let vv = h.SH.ex in
    let pp = h.SH.pi in
    let bb = extractFromGG yy h.SH.gm in
    let bb1 = L.map (fun (pp1,tt1) -> (PP.nf (dropRed (pp@pp1)),tt1)) bb in
    let bb2 = L.map BaseData.addPossibleAlloc bb1 in
    L.map (BaseData.dropUnObservable vv) bb2

  let extractFromSHL yy hh = 
    dropRed (L.flatten @@ L.map (extractFromSH yy) hh)

  let updateYY (dd : IndSys.t) yy : t = 
    let updateYY1 yy (pr,prm,hh) =
      match lookup yy pr with
      | None -> failwith @@ pr ^ " is not found"
      | Some(prm1,_) ->
		 let sub = zipLst prm (L.map var prm1) in
         let hh1 = L.map (SH.subst sub) hh in
         let bb = extractFromSHL yy hh1 in
         (pr,prm1,bb)
    in
    L.map (updateYY1 yy) dd

  let computeYY dd : t =
    let yyInit = L.map (fun (pr,prm,_) -> (pr,prm,[])) dd in
    let _yy = ref yyInit in
    let _yyNext = ref (updateYY dd yyInit) in
    while !_yy <> !_yyNext do
	  _yy := !_yyNext;
	  _yyNext := updateYY dd !_yyNext;
    done;
    !_yy
    
end
;;

let analyzeIS dd : YY.t =
  let yy = YY.computeYY dd in
  let getCommonData bb =
    let allcL = L.map snd bb in
    let allc = intersectL allcL in
    let ppp = L.map fst bb in
    let pp = intersectL ppp in
    [(pp,allc)]
  in
  L.map (fun (pr,prm,bb) -> (pr,prm,getCommonData bb)) yy
;;

(* Gamma の必要条件 (pure formula) を抽出する *)
let extractFromG yy g = 
  let (s,tt) = g in
  match s with
  | S.Pr(pr,uu) ->
     begin
       match YY.lookup yy pr with
       | None -> failwith @@ "extractFromG: NotFound -> " ^ pr
       | Some(prm,bb) ->
          let sub = zipLst prm uu in
          let (pp,ttAlloc) = L.hd (L.map (BaseData.subst sub) bb) in
          let ttAlloc1 = dropRed (tt@ttAlloc) in
          (pp,ttAlloc1)
     end
  | S.Emp -> if tt = [] then ([],[]) else ([T.Nil <.> T.Nil],[])
  | S.Pto(u,uu) ->
     let pp = L.map (fun t -> u <.> t) tt in
     (pp,[])
;;

let extractFromGG yy gg = 
  let ptL = L.map (extractFromG yy) gg in
  let pp1 = dropRed (PP.nf (L.flatten (L.map fst ptL))) in
  let ttL = L.map snd ptL in
  let pp2 = List_tailrec.mkAllMapGroup (fun (t,u) -> t <.> u) ttL in
  pp1 @ pp2
;;

let extractFromSH yy h =
  let pp0 = h.SH.pi in
  let gg = h.SH.gm in
  let pp1 = extractFromGG yy gg in
  dropRed (PP.nf (pp0@pp1))
;;
