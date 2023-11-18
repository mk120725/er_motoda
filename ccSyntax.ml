open Tools;;

module L = List;;
(*----------------------------------------------*)
(* Syntax for CyComp                            *)
(*----------------------------------------------*)
let renameStr rr v =
  match findItemOption v rr with
  | Some w -> w
  | None -> v
;;
let renameStrL rr vv = List.map (renameStr rr) vv
;;
let dropfilter cond = L.filter (fun x -> not(cond x))
;;
(*----------------------------------------------*)
(* Terms of Symbolic Heap                       *)
(*----------------------------------------------*)
module SHterm = struct
  (* Term expressions                   *)
  (* 't' is used for them               *)
  (* 'tt' is used for a list of them    *)
	
  type t =
  | Var of string
  | Nil
      
  let rec fv t =
	match t with
	| Nil -> []
	| Var v -> [v]
      
  let rec to_string t =
	match t with
	| Nil -> "Nil"
	| Var v -> v

  let print t = print_string (to_string t)

  let println t = print_string ((to_string t)^"\n")

  (* Variable Renaming *)
  let rec rename (rr : (string* string) list) t =
	match t with
	| Nil -> Nil
	| Var v -> Var (renameStr rr v)

  let rec subst (sub : (string * t) list) t =
	match t,sub with
	| _,[] -> t
	| Nil,_ -> Nil
	| Var x,_ ->
	  try
		L.assoc x sub
	  with
		Not_found -> t

  let compare t u =
	match t,u with
	| Nil,Nil -> 0
	| Nil,_ -> -1
	| _,Nil -> 1
	| Var v,Var w -> if v = w then 0 else if v < w then -1 else 1

  let extract t =
	match t with
	| Var v -> v
	| Nil -> failwith "Extract Failure: Nil"

end

(* Shortcuts for SHterms *)
module T = SHterm
let var v = T.Var v
let nil = T.Nil

module Subst = struct 
  type t = (string * T.t) list

  let to_string sub =
	let to_string1 (v,t) = v ^ "<-" ^ (T.to_string t) in
	let ss = string_of_list to_string1 "," sub in
	"[" ^ ss ^ "]"

  let println sub = print_endline (to_string sub)

end

(*--------------------------------------*)
(* A Pure Expression of Symbolic Heap   *)
(*--------------------------------------*)
type op = Eq | Neq

module SHpureExp = struct 
  (* Single Pure expressions         *)
  (* 'p' is used for them            *)

  type t = op * T.t * T.t

  let fv p =
    let (_,t1,t2) = p in
    dropRed ((T.fv t1) @ (T.fv t2))

  let to_string p =
    let (op,t1,t2) = p in
	let ts1 = T.to_string t1 in
	let ts2 = T.to_string t2 in
    match op with
    | Eq -> ts1 ^ " = " ^ ts2
    | Neq -> ts1 ^ " <> " ^ ts2

  let print (it : t) = print_string (to_string it)

  let println (it : t) = print_string ((to_string it)^"\n")
    
  let rename rr (p : t) : t =
    let (op,t1,t2) = p in
    let t1' = T.rename rr t1 in
	let t2' = T.rename rr t2 in
    (op,t1',t2')

  let subst (sub : Subst.t) (p : t) : t =
    let (op,t1,t2) = p in
    let t1' = T.subst sub t1 in
	let t2' = T.subst sub t2 in
    (op,t1',t2')

  let compare p1 p2 =
	let (op1,t1,u1) = p1 in
	let (op2,t2,u2) = p2 in
	match op1,op2,T.compare t1 t2 with
	| Eq,Neq,_ -> -1 (* (t1=u1) << (t2<>u2) *)
	| Neq,Eq,_ -> 1 (* (t1<>u1) >> (t2=u2) *)
	| _,_,1 -> 1 (* (t1##u1) >> (t2##u2) if t1 >> t2 *)
	| _,_,-1 -> -1 (* (t1##u1) << (t2##u2) if t1 << t2 *)
	| _,_,_ -> T.compare u1 u2 (* Otherwise *)
	  
  let nf (p : t) : t =
	let (op,t1,t2) = p in
	let tt = L.sort T.compare [t1;t2] in
	let t1' = L.nth tt 0 in
	let t2' = L.nth tt 1 in
	(op,t1',t2')
	  
end

(* Shortcuts for SHpureExps *)
module P = SHpureExp
  
let ( =.= ) t1 t2 : P.t = (Eq,t1,t2)
  
let ( <.> ) t1 t2 : P.t = (Neq,t1,t2)
  
let top = (nil =.= nil)
  
let bot = (nil <.> nil)


(*----------------------------------------------*)
(* Pure formulas                                *)
(*----------------------------------------------*)
module SHpure = struct
(* 'pp' is used *)

  type t = P.t list

  let fv (pp : t) = 
    L.concat (L.map P.fv pp)

  let to_string (pp : t) =
    string_of_list P.to_string " & " pp

  let print (pp : t) = print_string (to_string pp)
    
  let println (pp : t) = print_endline (to_string pp)

  let rename rr (pp : t) : t = 
	L.map (P.rename rr) pp

  let subst sub (pp : t) : t = L.map (P.subst sub) pp

  let nf pp =
	let pp1 = L.map P.nf pp in
	dropRed (L.sort P.compare pp1)

  let splitEqNeq pp =
	let rec aux eq neq pp1 =
	  match pp1 with
	  | [] -> (L.rev eq,L.rev neq)
	  | ((Eq,t,u) as p)::pp2 -> aux (p::eq) neq pp2
	  | ((Neq,t,u) as p)::pp2 -> aux eq (p::neq) pp2
	in
	aux [] [] pp
	  
  let entlcheck (pp : t) (ppp : t list) =
	let pp1 = nf pp in
	let isTrivEq (op,t,u) = op = Eq && t = u in
	let isTrivNeq (op,t,u) = op = Neq && t = u in
	let isEq (op,_,_) = op = Eq in
	let isNeq (op,_,_) = op = Neq in
	let hasEq pp = L.exists isEq pp in
	let hasTrivNeq pp = L.exists isTrivNeq pp in
	let (ppEq,ppNeq) = (L.filter isEq pp1, L.filter isNeq pp1) in
	let _ppEq = ref ppEq in
	let _ppNeq = ref ppNeq in
	let _ppp = ref ppp in
	for i = 0 to L.length ppEq - 1 do
	  match L.nth !_ppEq i with
		  | (_,u,T.Var x) ->
			 begin
			   _ppEq := subst [(x,u)] !_ppEq;
			   _ppNeq := subst [(x,u)] !_ppNeq;
			   _ppp := L.map (subst [(x,u)]) !_ppp;
			 end
		  | (_,_,_) -> ()
	done;
	match hasTrivNeq !_ppNeq with
	| true -> true (* if LHS has triv.neq, entl is trivially valid *)
	| false ->
	   begin
		 _ppp := L.map (dropfilter isTrivEq) !_ppp;
		 _ppp := dropfilter hasEq !_ppp;
		 _ppp := dropfilter hasTrivNeq !_ppp;
		 _ppp := L.map nf !_ppp;
		 _ppNeq := nf !_ppNeq;
		 _ppp := L.filter (fun pp -> subset pp !_ppNeq) !_ppp;
		 if !_ppp = [] then false else true
	   end

  let satcheck pp =	not (entlcheck pp [])

  let simpleCheckCons (pp : t) =
	(* Simple Consistent Checking                   *)
	(* It checks whether t = u and t <> u are in pp *)
	let pp1 = nf pp in
	let (eq,neq) = splitEqNeq pp1 in
	let res = ref true in
	begin
	  try
		for i = 0 to L.length eq - 1 do
		  let (_,t,u) = L.nth eq i in
		  if L.mem (Neq,t,u) neq then raise Exit else ()
		done;
	  with Exit -> res := false
	end;
	!res
	  
end

(* Short cuts for SHpures *)
module PP = SHpure
  
let ( =..= ) (ts1 : T.t list) (ts2 : T.t list) : PP.t = 
  let zipts = zipLst ts1 ts2 in
  List.map (fun (x,y) -> x =.= y) zipts

module SHspatExp = struct
(* Single Spatial expressions		*)
(* 's' is used for them			*)

  type t =
	| Emp
	| Pto of T.t * T.t list
	| Pr of string * T.t list

  let fv s = match s with
	| Emp -> []
    | Pto(t,tt) ->
      let tt1 = t::tt in
      dropRed(L.concat (L.map T.fv tt1))
    | Pr(_,tt) -> 
      dropRed(L.concat (L.map T.fv tt))
		
  let to_string s = match s with
	| Emp -> "Emp"
    | Pto(t,tt) ->
      let t' = T.to_string t in
      let tt' = string_of_list T.to_string "," tt in
      t'^" -> ("^tt'^")"
    | Pr(pr,tt) -> 
      let tt' = string_of_list T.to_string "," tt in
      pr^"("^tt'^")"

  let print (s : t) = print_string (to_string s)
    
  let println (s : t) = print_string ((to_string s)^"\n")

  let rename rr (s : t) : t = match s with
	| Emp -> Emp
    | Pto(t,tt) ->
      let t' = T.rename rr t in
      let tt' = L.map (T.rename rr) tt in
      Pto(t',tt')
    | Pr(pr,tt) ->
      let tt' = L.map (T.rename rr) tt in
      Pr(pr,tt')

  let subst (sub : Subst.t) s = match s with
	| Emp -> Emp
    | Pto(t,tt) ->
      let t' = T.subst sub t in
      let tt' = L.map (T.subst sub) tt in
      Pto(t',tt')
    | Pr(pr,tt) ->
       let tt' = L.map (T.subst sub) tt in
       Pr(pr,tt')

  let getRootsPto (s : t) =
	match s with
	| Pto(t,_) -> [t]
	| _ -> []

  let getRootsPr (s : t) =
	match s with
	| Pr(_,t::_) -> [t]
	| _ -> []

  let getRoots (s : t) = (getRootsPto s) @ (getRootsPr s)

  let ptoSize (s : t) =
	match s with
	| Pto(_,tt) -> Some (L.length tt)
	| _ -> None
	
end

(* Short cuts for SHspatExps *)
module S = SHspatExp
  
let ( -.> ) t tt = S.Pto(t,tt)
  
let ipred(p,tt) = S.Pr(p,tt)

module SHgammaExp = struct
	(* (spat,[x;y]) means spat & Dn(x,y) *)
	(* g is used *)
  type t = S.t * T.t list

  let fv (g : t) =
	let (s,tt) = g in
	let s1 = S.fv s in
	let tt1 = L.concat (L.map T.fv tt) in
	dropRed(s1 @ tt1)

  let to_string (g : t) =
	let (s,tt) = g in
	let s1 = S.to_string s in
	match tt with
	| [] -> s1
	| _ -> 
	   let tt1 = L.map T.to_string tt in
	   let tt2 = concatStrLComma tt1 in
	   let tt3 = "<" ^ tt2 ^ ">" in
	   s1 ^ "&" ^ tt3
		 
  let print (g : t) = print_string (to_string g)
    
  let println (g : t) = print_string ((to_string g)^"\n")

  let rename rr (g : t) : t =
	let (s,tt) = g in
	let s1 = S.rename rr s in
	let tt1 = L.map (T.rename rr) tt in
	(s1,tt1)

  let subst sub (g : t) =
	let (s,tt) = g in
	let s1 = S.subst sub s in
	let tt1 = L.map (T.subst sub) tt in
	(s1,tt1)

  let forget (g : t) = fst g

  let getRootsPto (g : t) =
	let (s,_) = g in
	S.getRootsPto s
	
  let getRootsPr (g : t) =
	let (s,_) = g in
	S.getRootsPr s

  let getRoots (g : t) = (getRootsPto g) @ (getRootsPr g)

  let getCells (g : t) = snd g

  let getRootCells (g : t) = dropRed ((getRoots g) @ (getCells g))

  let ptoSize (g : t) = S.ptoSize (fst g)
	
  let nf (g : t) : t = 
	let (s,tt) = g in
	let tt1 = setminus tt (S.getRoots s) in
	let tt2 = dropRed (L.sort T.compare tt1) in
	(s,tt2)

  let extractGrp (g : t) =
	(* It returns ([(t1,u1);(t2,u2)],[[x;y];[z]])  *)
	(* Meaning: t1=u1 & t2=u2 & x <> z & y <> z    *)
	let (s,tt) = g in
	match s with
	| S.Emp -> ([],[])
	| S.Pto(u,_) ->
	   let eqs = L.map (fun t -> (u,t)) tt in
	   (eqs,u::tt)
	| S.Pr(_,uu) ->
	   let u = L.hd uu in
	   ([],u::tt)
	  
end

module G = SHgammaExp

module SHgamma = struct
  (* gg is used *)
  type t = G.t list

  let fv (gg : t) = 
    L.concat (L.map G.fv gg)

  let to_string (gg : t) =
    string_of_list G.to_string " * " gg

  let print (gg : t) = print_string (to_string gg)
    
  let println (gg : t) = print_endline (to_string gg)

  let rename rr (gg : t) : t = L.map (G.rename rr) gg

  let subst sub (gg : t) : t = L.map (G.subst sub) gg
	
  let forget (gg : t) = L.map G.forget gg

  let getRootsPto (gg : t) = L.flatten (L.map G.getRootsPto gg)
	
  let getRootsPr (gg : t) = L.flatten (L.map G.getRootsPr gg)

  let getRoots (gg : t) = (getRootsPto gg) @ (getRootsPr gg)

  let getCells (gg : t) = L.flatten (L.map G.getCells gg)

  let getRootCells (gg : t) = dropRed ((getRoots gg) @ (getCells gg))

  let rec ptoSize (gg : t) =
	match gg with
	| [] -> None
	| g::gg1 ->
	   match G.ptoSize g with
	   | None -> ptoSize gg1
	   | Some _ as q -> q
	
  let nf (gg : t) : t = L.map G.nf gg

  let extractGrp (gg : t) =
	let _eqs = ref [] in
	let _gps = ref [] in
	for i = 0 to L.length gg - 1 do
	  let g = L.nth gg i in
	  let (eqs,grp) = G.extractGrp g in
	  _eqs := eqs @ !_eqs;
	  _gps := grp :: !_gps;
	done;
	(!_eqs,!_gps)
	
end

module GG = SHgamma

(*----------------------------------------------*)
(* Symbolic Heaps                               *)
(*----------------------------------------------*)
module SH = struct
  (* h is used *)
  type t =
	{
	  mutable ex : string list;
	  mutable up : T.t list;
	  mutable out : T.t list;
	  mutable pi : PP.t;
	  mutable gm : GG.t
	}

  let create xx yy oo pp ss = {ex=xx; up=yy; out=oo; pi=pp; gm=ss}

  let clone h = create h.ex h.up h.out h.pi h.gm

  let mkDummy () = create [] [] [] [] [] 
	
  let decomp h = (h.ex,h.up,h.out,h.pi,h.gm)

  let av h =
	let vv0 = h.ex in
	let vv1 = L.flatten (L.map T.fv (h.up @ h.out)) in
	let vv2 = PP.fv h.pi in
	let vv3 = GG.fv h.gm in
	dropRed (vv0 @ vv1 @ vv2 @ vv3)
	
  let fv h = setminus (av h) h.ex
	
  let to_string h =
	let exPart = match h.ex with
	  | [] -> ""
	  | xx -> "Ex:[" ^ (concatStrLComma xx) ^"]" in
	let upPart = match h.up with
	  | [] -> ""
	  | yy -> "up:[" ^ (concatStrLComma (L.map T.to_string yy)) ^ "], " in
	let outPart = match h.out with
	  | [] -> ""
	  | oo -> "out:[" ^ (concatStrLComma (L.map T.to_string oo)) ^ "], " in
	let piPart = "pi:[" ^ (PP.to_string h.pi) ^ "], " in
	let gmPart = "gm:[" ^ (GG.to_string h.gm) ^ "]" in
	exPart ^ "("^ upPart ^ outPart ^ piPart ^ gmPart ^ ")"

  let print (h : t) = print_string (to_string h)
    
  let println (h : t) = print_string ((to_string h)^"\n")

  let alpha_conv vv h =
	let xx = h.ex in
	let rec mkRename vv1 xx1 = match xx1 with
	  | [] -> []
	  | x::xx2 ->
		 if List.mem x vv1 then
		   let x' = genFreshVar x vv1 in
		   (x,x')::(mkRename (x'::vv1) xx2)
		 else
		   mkRename vv1 xx2
	in
	let rr = mkRename vv xx in
	create
	  (renameStrL rr xx)
	  (List.map (T.rename rr) h.up)
	  (List.map (T.rename rr) h.out)
	  (PP.rename rr h.pi)
	  (GG.rename rr h.gm)
	  
  let subst sub (h : t) : t =
	let h1 = clone h in
	let vv = List.flatten (List.map (fun (_,t) -> T.fv t) sub) in
	let h2 = alpha_conv vv h1 in
	let xx = h2.ex in
	let sub1 = List.filter (fun (x,_) -> not(List.mem x xx)) sub in
	h2.up <- List.map (T.subst sub1) h2.up;
	h2.out <- List.map (T.subst sub1) h2.out;
	h2.pi <- PP.subst sub1 h2.pi;
	h2.gm <- GG.subst sub1 h2.gm;
	h2

  let getRootsPto (h : t) =
	let zz = L.map var h.ex in
	setminus (GG.getRootsPto h.gm) zz
	
  let getRootsPr (h : t) = 
	let zz = L.map var h.ex in
	setminus (GG.getRootsPr h.gm) zz
	  
  let getRoots (h : t) = (getRootsPto h) @ (getRootsPr h)

  let getCells (h : t) = 
	let zz = L.map var h.ex in
	setminus (GG.getCells h.gm) zz

  let getRootCells (h : t) = dropRed ((getRoots h) @ (getCells h))	

  let ptoSize (h : t) = GG.ptoSize h.gm
	
  let nf (h : t) : t =
	let h' = alpha_conv [] h in
	h'.ex <- dropRed (L.sort compare h'.ex);
	h'.up <- dropRed (L.sort T.compare h'.up);
	h'.up <- L.filter (fun t -> T.Nil <> t) h'.up;
	h'.out <- dropRed (L.sort T.compare h'.out);
	h'.out <- L.filter (fun t -> T.Nil <> t) h'.out;
	h'.pi <- PP.nf h'.pi;
	h'.gm <- GG.nf h'.gm;
	h'

end



(*----------------------------------------------*)
(* Multi-conclusion Entailments                 *)
(*----------------------------------------------*)
module Entl = struct
  (* Entailment has the form sh1 |- sh21 | sh22 | ... | sh2m *)
  (* Basically, the quantifier-part of lhs is assumed to be empty *)
  (* ([],P,G) |- (v1,P1,G1),...,(vn,Pn,Gn)	*)
  (* 'e' is used for them *)

  type t =
	{
	  mutable name : string;
	  mutable ant : SH.t;
	  mutable suc : SH.t list;
	}

  let fv (e : t) =
	dropRed (L.flatten ((SH.fv e.ant) :: (L.map SH.fv e.suc)))

  let av (e : t) =
	dropRed (L.flatten ((SH.av e.ant) :: (L.map SH.av e.suc)))
	  
  let mkDummy () = {name = ""; ant = SH.mkDummy (); suc = [] }
	
	  
  let create id h hh = {name = id; ant = h; suc = hh}

  let clone e = create e.name (SH.clone e.ant) (L.map SH.clone e.suc)

  let decomp e = (e.name,e.ant,e.suc)

  let to_string (e : t) =
    let hS = SH.to_string e.ant in
    let hhS =
	  if e.suc = [] then "\n| "
	  else L.fold_right (fun h s -> "\n| " ^ (SH.to_string h) ^ s) e.suc "" in
    let entlStr = hS ^ hhS in
	match e.name with
	| "" -> entlStr
	| _ -> "[" ^ e.name ^ "] " ^ entlStr

  let to_string_id (e : t) = "[" ^ e.name ^ "] "
	   
  let print (e : t) = print_string (to_string e)

  let println (e : t) = print_string ((to_string e)^"\n")

  let subst sub (e : t) =
	let e1 = clone e in
	e1.ant <- SH.subst sub e1.ant;
	e1.suc <- L.map (SH.subst sub) e1.suc;
	e1

  let alpha_conv vv (e : t) =
    let e1 = clone e in
    e1.ant <- SH.alpha_conv vv e1.ant;
    e1.suc <- L.map (SH.alpha_conv vv) e1.suc;
    e1
    
  let nf (e : t) =
	let e' = clone e in
	e'.ant <- SH.nf e.ant;
	e'.suc <- dropRed (L.map SH.nf e.suc);
	e'
	
end


module E = Entl

  
module GrpSHs = struct
  (* SHs in a same group *)
  type t =
	{
	  mutable ex : string list;
	  mutable up : T.t list;
	  mutable out : T.t list;
	  mutable body : SH.t list
	}
  (* Ex xyz. up(y)&out(z)& {sh1} * {sh2} *)
	  
  let to_string kk =
	let exPart = match kk.ex with
	  | [] -> ""
	  | xx -> "Ex:[" ^ (concatStrLComma xx) ^"]" in
	let upPart = match kk.up with
	  | [] -> ""
	  | yy -> "up:[" ^ (concatStrLComma (L.map T.to_string yy)) ^ "], " in
	let outPart = match kk.out with
	  | [] -> ""
	  | oo -> "out:[" ^ (concatStrLComma (L.map T.to_string oo)) ^ "], " in
	let body = string_of_list (fun h -> "{" ^ (SH.to_string h) ^ "}") " * " kk.body in
	exPart ^ "("^ upPart ^ outPart ^ body ^ ")"

  let create xx yy oo hh = { ex=xx; up=yy; out=oo; body=hh}

  let clone kk = create kk.ex kk.up kk.out kk.body
	
  let decomp kk = (kk.ex,kk.up,kk.out,kk.body)

  let flatten (kk : t) : SH.t list =
	let exkk = kk.ex in
	let upkk = kk.up in
	let outkk = kk.out in
	let flatten_one h =
	  { SH.ex = dropRed (L.sort compare (exkk @ h.SH.ex));
		SH.up = dropRed (L.sort T.compare (upkk @ h.SH.up));
		SH.out = dropRed (L.sort T.compare (outkk @ h.SH.out));
		SH.pi = h.SH.pi;
		SH.gm = h.SH.gm
	  }
	in
	L.map flatten_one kk.body

  let fv kk = dropRed (L.flatten (L.map SH.fv (flatten kk)))

  let print (h : t) = print_string (to_string h)

  let println (h : t) = print_string ((to_string h)^"\n")
(*
  let hd (h : t) = { ex = h.ex; up = h.up; out = h.out; body = [L.hd h.body] }

  let tl (h : t) = { ex = h.ex; up = h.up; out = h.out; body = L.tl h.body }	
*)
  let filterRoots tt (h : t) =
	let hasRoots sh = intersect tt (SH.getRoots sh) <> [] in
	{ ex = h.ex; up = h.up; out = h.out; body = L.filter hasRoots h.body}
	
  let fromSingleGroup (h : t) =
	let h1 = SH.clone (L.hd h.body) in
	h1.SH.ex <- h.ex @ h1.SH.ex;
	h1.SH.up <- h.up @ h1.SH.up;
	h1.SH.out <- h.out @ h1.SH.out;
	h1
	
end

  
module GrpEntl = struct

  type t =
	{
	  mutable name : string;
	  mutable ant : SH.t;
	  mutable suc : GrpSHs.t list
	}
  (* Pi&Spat |- Ex xyz. up(y)&out(z)& {sh1} * {sh2} *)
	  
  let to_string (ge : t) =
    let hS = SH.to_string ge.ant in
    let kkkS =
	  if ge.suc = [] then "\n| "
	  else L.fold_right (fun h s -> "\n| " ^ (GrpSHs.to_string h) ^ s) ge.suc ""
	in
	match ge.name with
	| "" -> hS ^ kkkS
	| _ -> "[" ^ ge.name ^ "] " ^ hS ^ kkkS

  let to_string_id (ge : t) = "[" ^ ge.name ^ "]"

  let to_string_id_sub (ge : t) sub =
    let subst_id = string_of_int sub in
    "[" ^ ge.name ^ ":subsetID=" ^ subst_id ^ "]"
                            
  let print (ge : t) = print_string (to_string ge)

  let println (ge : t) = print_string ((to_string ge)^"\n")

  let decomp ge = (ge.name, ge.ant, ge.suc)

  let create id h kk = { name = id; ant = h; suc = kk }
	
  let clone ge = { name = ge.name; ant = SH.clone ge.ant; suc = L.map GrpSHs.clone ge.suc }
	
  let flatten ge : Entl.t =
	let (id,h,kkk) = decomp ge in
	let hh = L.flatten (L.map GrpSHs.flatten kkk) in
	{ Entl.name = id; Entl.ant = h; Entl.suc = hh }

  let fv ge = Entl.fv (flatten ge)

  let numGroup ge = L.length ge.ant.SH.gm
	
  let isSingleGroup ge = numGroup ge = 1

  let fromSingleGroup (ge : t) =
	{
	  Entl.name = ge.name;
	  Entl.ant = ge.ant;
	  Entl.suc = L.map GrpSHs.fromSingleGroup ge.suc
	}	

  (* make subset-entailment of ge according to sub *)
  (* 'mkSubEntl (F|-G1,G2,G3) 0b110' returns F|-G1,G2 *)
  let mkSubEntl ge sub =
    let ge1 = clone ge in
    let size = List.length ge.suc in
    let _sub = ref sub in
    let _suc = ref [] in
    for i = 0 to size - 1 do
      begin
        match !_sub mod 2 = 1 with
        | true ->
           _suc := (List.nth ge.suc (size - i - 1)) :: !_suc
        | false -> ()
      end;
      _sub := !_sub lsr 1
    done;
    ge1.suc <- !_suc;
    ge1
    
end

module GE = GrpEntl

(*----------------------------------------------*)
(* Inductive Definitions                        *)
(*----------------------------------------------*)
module IndDef = struct
  (* ("P",[x;y],[sh_1;..;sh_n])          *)
  (* 'd' is used for them                *)
  type t = string * string list * SH.t list

  let to_string (d : t) =
	let (pr,vv,def) = d in
	let prm = concatStrLComma vv in
	let head = pr ^ "("^prm^")" in
	let body = string_of_list SH.to_string " | " def in
	head ^ " := " ^ body

  let print (d : t) = print_string (to_string d)

  let println (d : t) = print_string ((to_string d)^"\n")

  let prmconv (d : t) : t =
	let (pr,prms,defL) = d in
    let len = L.length prms in
    let numL = L.rev (genLst len) in
    let prms' = L.map (fun n -> "%"^(string_of_int n)) numL in
    let sub = zipLst prms (L.map (fun v -> T.Var v) prms') in
	let updateBody h =
	  let h' = SH.clone h in
	  h'.SH.out <- L.map (T.subst sub) h.SH.out;
	  h'.SH.up <- L.map (T.subst sub) h.SH.up;
	  h'.SH.pi <- PP.subst sub h.SH.pi;
	  h'.SH.gm <- GG.subst sub h.SH.gm;
	  h'
	in
    let defL' = L.map updateBody defL in
    (pr,prms',defL')

  let spatconv (d : t) : t =
	let (pr,prms,defL) = d in
    let defL' = L.map SH.nf defL in
    (pr,prms,defL')

  let prms (d : t) = let (_,prms,defL) = d in prms

  let ptoSize (d : t) =
	let (_,_,hh) = d in
	let rec aux hh1 = 
	  match hh1 with
	  | [] -> None
	  | h::hh2 ->
		 match SH.ptoSize h with
		 | None -> aux hh2
		 | _ as q -> q
	in
	aux hh

end


(*----------------------------------------------*)
(* Inductive System (set of Ind.Defs)           *)
(*----------------------------------------------*)
module IndSys = struct

  type t = IndDef.t list

  let to_string (dd : t) =
	string_of_list IndDef.to_string "\n" dd

  let print (dd : t) = print_string (to_string dd)

  let println (dd : t) = print_string ((to_string dd)^"\n")

  let nf (is : t) : t = L.map (fun d -> IndDef.spatconv(IndDef.prmconv d)) is

  let lookup (dd : t) vQ =
	let _ans = ref [] in
	for i = 0 to L.length dd - 1 do
	  let (vP,prm,defL) = L.nth dd i in
	  if vP = vQ then _ans := (prm,defL) :: !_ans else ()
	done;
	!_ans

  let lookupParamLen dd vQ =
	match lookup dd vQ with
	| [] -> []
	| (prm,_)::_ -> [ L.length prm ]

  let lookupDefL dd vQ =
	match lookup dd vQ with
	| [] -> []
	| (_,defL)::_ -> defL

  let kmax (dd : t) =
	let kk = List.map (fun d -> L.length (IndDef.prms d)) dd in
	let _mx = ref 0 in
	for i = 0 to List.length kk - 1 do
	  if !_mx < List.nth kk i then _mx := List.nth kk i else ()
	done;
	!_mx

  let rec ptoSize (dd : t) =
	match dd with
	| [] -> 0
	| d::dd1 ->
	   match IndDef.ptoSize d with
	   | None -> ptoSize dd1
	   | Some n -> n
	  
end

module IS = IndSys

(*----------------------------------------------*)
(* Program System                               *)
(*----------------------------------------------*)
module ProgSys = struct
  (* input file for satcheck *)
  type t = Entl.t * IndSys.t

  let to_string (ps : t) =
	let (e,dd) = ps in
	let eS = Entl.to_string e in
	let ddS = IndSys.to_string dd in
	eS ^ "\n\n---\n" ^ ddS

  let print (ps : t) = print_string (to_string ps)

  let println (ps : t) = print_string ((to_string ps)^"\n")	

  let nf (ps : t) : t =
	let (e,dd) = ps in
	(e,IS.nf dd)
	
end

module PS = ProgSys




(*-----------------------------------*)
(* Common Utilities for Entlcheck    *)
(*-----------------------------------*)
let mkNeqAll tt1 tt2 : PP.t =
  (* mkNeqAll [t1;t2] [u1;u2] returns *)
  (* t1 <> u1 & t1 <> u2 & t2 <> u1 & t2 <> u2 *)
  let _ans = ref [] in
  for i = 0 to L.length tt1 - 1 do
	let t1 = L.nth tt1 i in
	for j = 0 to L.length tt2 - 1 do
	  let t2 = L.nth tt2 j in
	  if t1 = t2 then ()
	  else
		_ans := (t1 <.> t2) :: !_ans
	done;
  done;
  !_ans
;;	

let mkNeqAll1 tt : PP.t =
  let _ans = ref [] in
  let tt' = L.sort T.compare tt in
  for i = 0 to L.length tt' - 1 do
	let t1 = L.nth tt' i in
	for j = i+1 to L.length tt' - 1 do
	  let t2 = L.nth tt' j in
	  _ans := (t1 <.> t2) :: !_ans
	done;
  done;
  !_ans
;;

let mkEqNeqAll1 tt : PP.t list =
  (* mkEqNeqAll [t1;t2;t3] returns *)
  (* t1 <> t2 & t2 <> t3 & t3 <> t1 *)
  (* t1 = t2 & t2 <> t3 & t3 <> t1 *)
  (* t1 <> t2 & t2 = t3 & t3 <> t1 *)
  (* t1 <> t2 & t2 <> t3 & t3 = t1 *)
  (* t1 = t2 & t2 = t3 *)
  let _ans = ref [[]] in
  for i = 0 to L.length tt - 1 do
	let t1 = L.nth tt i in
	for j = i+1 to L.length tt - 1 do
	  let t2 = L.nth tt j in
	  if t1 = t2 then ()
	  else
		begin
		  let ppa = L.filter PP.satcheck (L.map (fun pp -> (t1 <.> t2)::pp) !_ans) in
		  let ppb = L.filter PP.satcheck (L.map (fun pp -> (t1 =.= t2)::pp) !_ans) in
		  _ans := ppa @ ppb
		end
	done;
  done;
  !_ans
;;

let mkDiffSameOne a (xxDiff,xxSame) =
  (* mkDiffSame xyz ab returns [(xyz;ax); (xyz;ay); (xyz;az); (xyza; )] *)
  (* (xyz,ax) means x <> y & y <> z & z <> x & a = x *)
  let _res = ref [] in
  for i = 0 to L.length xxDiff - 1 do
	let x = L.nth xxDiff i in
	_res := (xxDiff,(a,x)::xxSame) :: !_res
  done;
  _res := (a::xxDiff,xxSame) :: !_res;
  !_res
;;

let updateDiffSame a xxDiffSameL = L.flatten (L.map (mkDiffSameOne a) xxDiffSameL)
;;

let mkDiffSame xxDiff xxOther =
  (* xxDiff = [x;y;z] means x,y,z are different each other *)
  (* xxOther = [a;b] means a,b are other elements          *)
  (* mkDiff [x;y;z] [a;b] returns (bxyz,ay) etc            *)
  (* It means <>{b,x,y,z} and a = y                        *)
  let xxDiffSameL = ref [ (xxDiff,[]) ] in
  for i = 0 to L.length xxOther - 1 do
	let x = L.nth xxOther i in
	xxDiffSameL := updateDiffSame x !xxDiffSameL
  done;
  !xxDiffSameL
;;  

let mkEqNeqAll2 ttAlloc ttOther =
  let pairsDiffEq = mkDiffSame (nil::ttAlloc) ttOther in
  let pairToPP (xxDiff,eqs) =
	(mkNeqAll1 xxDiff) @ (L.map (fun (t,u) -> t =.= u) eqs)
  in
  L.map pairToPP pairsDiffEq
;;	  
