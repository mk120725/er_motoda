open Tools;;
open Ratio;;

let renameStr rr v =
  match findItemOption v rr with
  | Some w -> w
  | None -> v
;;
let renameStrL rr vv = List.map (renameStr rr) vv
;;
let dropfilter cond = List.filter (fun x -> not(cond x))
;;
let rec add t ts =
  match ts with
  | [] -> [t]
  | t1::rest -> if t=t1 then (add t rest) else t1::(add t rest)
;;
let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | t::rest -> union rest (add t l2)
;;
let rec map_union f l =
  match l with
  | [] -> []
  | x::rest -> union (f x) (map_union f rest)

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
		List.assoc x sub
	  with
		Not_found -> t

  let compare t u =
	match t,u with
	| Nil,Nil -> 0
	| Nil,_ -> -1
	| _,Nil -> 1
	| Var v,Var w -> if v = w then 0 else if v < w then -1 else 1

  let rec fv t =
	match t with
	| Nil -> []
	| Var v -> [v]

  let rec fvs = map_union fv
end

(* Shortcuts for SHterms *)
let var v = SHterm.Var v
let nil = SHterm.Nil


(* substitutions *)
(* [("v1",t1); ("v2",t2);...] *)
module Subst = struct 
  type t = (string * SHterm.t) list

  let to_string sub =
	let to_string1 (v,t) = v ^ "<-" ^ (SHterm.to_string t) in
	let ss = string_of_list to_string1 "," sub in
	"[" ^ ss ^ "]"

  let println sub = print_endline (to_string sub)
end



(* spatial atoms *)
module SHspatExp = struct
(* Single Spatial expressions		*)
(* 's' is used for them			*)
  type t =
	| Emp
	| Pto of SHterm.t * SHterm.t list
	| Pr of string * SHterm.t list
	| Lab of string * ratio

  let rec fv s =
    match s with
    | Emp -> []
    | Pto(t,ts) -> union (SHterm.fv t) (SHterm.fvs ts)
    | Pr(pr,ts) -> (SHterm.fvs ts)
    | Lab(a,p) -> []

  let rec lab p =
    match p with
    | Emp -> []
    | Pto(t,ts) -> [] 
    | Pr(pr,ts) -> []
    | Lab(a,p) -> [a]

  let to_string s = match s with
    (* todo *)
    | Emp -> "Emp"
    | Pto(t,tt) ->
      let t' = SHterm.to_string t in
      let tt' = string_of_list SHterm.to_string "," tt in
      t'^" -> ("^tt'^")"
    | Pr(pr,tt) -> 
      let tt' = string_of_list SHterm.to_string "," tt in
      pr^"("^tt'^")"
    | Lab(lab,rat) ->
       lab ^ "^(" ^ (string_of_ratio rat) ^ ")"

  let print (s : t) = print_string (to_string s)
    
  let println (s : t) = print_string ((to_string s)^"\n")	

  let rec rename (rr : (string* string) list) t =
	match t with
	| Emp -> Emp
	| Pto(t,ts) -> Pto(SHterm.rename rr t, List.map (SHterm.rename rr) ts)
	| Pr(pr,ts) -> Pr(pr, List.map (SHterm.rename rr) ts)
	| Lab(a,p) -> Lab(a,p)

  let rec subst (sub : (string * SHterm.t) list) t =
	match t with
	| Emp -> Emp
	| Pto(t,ts) -> Pto(SHterm.subst sub t, List.map (SHterm.subst sub) ts)
	| Pr(pr,ts) -> Pr(pr, List.map (SHterm.subst sub) ts)
	| Lab(a,p) -> Lab(a,p)

  let ccp (s : t) =
    match s with
    | Lab(a,p) -> false
    | _ -> true

  let empp (s : t) =
    | Emp -> true
    | Pr(p,ts) ->
       (match ts with (* p must be ls *)
       | Nil::Nil::[] -> true
       | _ -> false
       )
    | _ -> false
  
end

(* Short cuts for SHspatExps *)
let ( -.> ) t tt = SHspatExp.Pto(t,tt)
let ipred(p,tt) = SHspatExp.Pr(p,tt)

(* spatial formulas *)
module SHspat = struct
  type t =
    | Emp
    | SAtom of SHspatExp.t
    | SCon of t * t
    | WCon of t * t

  let rec fv s =
    match s with
    | Emp -> []
    | SAtom(a) -> SHspatExp.fv a
    | SCon(s1,s2) -> union (fv s1) (fv s2)
    | WCon(s1,s2) -> union (fv s1) (fv s2)

  let rec lab s =
    match s with
    | Emp -> []
    | SAtom(a) -> SHspatExp.lab a
    | SCon(s1,s2) -> union (lab s1) (lab s2)
    | WCon(s1,s2) -> union (lab s1) (lab s2)
  
  let rec to_string (sigma : t) =
    match sigma with
    | Emp -> "Emp"
    | SAtom s -> SHspatExp.to_string s
    | SCon (s1,s2) -> "(" ^ (to_string s1) ^ ") * (" ^ (to_string s2) ^ ")"
    | WCon (s1,s2) -> "(" ^ (to_string s1) ^ ") w* (" ^ (to_string s2) ^ ")"

  let print (g : t) = print_string (to_string g)
    
  let println (g : t) = print_string ((to_string g)^"\n")

  let rec rename (rr : (string* string) list) t =
    match t with
    | Emp -> Emp
    | SAtom(t) -> SAtom(SHspatExp.rename rr t) 
    | SCon(t1,t2) -> SCon(rename rr t1, rename rr t2)
    | WCon(t1,t2) -> WCon(rename rr t1, rename rr t2)

  let rec subst (sub : (string * SHterm.t) list) t =
    match t with
    | Emp -> Emp
    | SAtom(t) -> SAtom(SHspatExp.subst sub t) 
    | SCon(t1,t2) -> SCon(subst sub t1, subst sub t2)
    | WCon(t1,t2) -> WCon(subst sub t1, subst sub t2)

  let rec ccp (s : t) =
    match s with
    | Emp -> true
    | SAtom(a) -> SHspatExp.ccp a
    | SCon(s1,s2) -> ccp s1 && ccp s2
    | WCon(s1,s2) -> false

  let rec empp (s : t) =
    match s with
    | Emp -> true
    | SAtom(a) -> SHspatExp.empp a
    | SCon(s1,s2) -> empp s1 && empp s1
    | WCon(s1,s2) -> false
    
end

	

(*--------------------------------------*)
(* A Pure Expression of Symbolic Heap   *)
(*--------------------------------------*)

(* pure atoms *)
module SHpureExp = struct
  type t =
    | Eq of SHterm.t * SHterm.t
    | NEq of SHterm.t * SHterm.t
    | At of string * SHspat.t

  let rec fv p =
    match p with
    | Eq(t1,t2) -> union (SHterm.fv t1) (SHterm.fv t2)
    | NEq(t1,t2) -> union (SHterm.fv t1) (SHterm.fv t2)
    | At(a,s) -> SHspat.fv s

  let rec lab p =
    match p with
    | Eq(t1,t2) -> []
    | NEq(t1,t2) -> []
    | At(a,s) -> [a]

  let to_string p =
    match p with
    | Eq (ts1,ts2) -> (SHterm.to_string ts1) ^ " = " ^ (SHterm.to_string ts2)
    | NEq (ts1,ts2) -> (SHterm.to_string ts1) ^ " <> " ^ (SHterm.to_string ts2)
    | At (lab,sigma) -> "@(" ^ lab ^ "," ^ (SHspat.to_string sigma) ^ ")"

  let print (it : t) = print_string (to_string it)

  let println (it : t) = print_string ((to_string it)^"\n")

  let rec rename (rr : (string* string) list) t =
    match t with
    | Eq(t1,t2) -> Eq(SHterm.rename rr t1, SHterm.rename rr t2)
    | NEq(t1,t2) -> NEq(SHterm.rename rr t1, SHterm.rename rr t2)
    | At(a,s) -> At(a,SHspat.rename rr s)

  let rec subst (sub : (string * SHterm.t) list) t =
    match t with
    | Eq(t1,t2) -> Eq(SHterm.subst sub t1, SHterm.subst sub t2)
    | NEq(t1,t2) -> NEq(SHterm.subst sub t1, SHterm.subst sub t2)
    | At(a,s) -> At(a,SHspat.subst sub s)

  let ccp (it : t) =
    match it with
    | At(a,s) -> false
    | _ -> true
end

(* Shortcuts for SHpureExps *)
  
let ( =.= ) t1 t2 : SHpureExp.t = Eq(t1,t2)
  
let ( <.> ) t1 t2 : SHpureExp.t = NEq(t1,t2)
  
let top = (nil =.= nil)
  
let bot = (nil <.> nil)


(*----------------------------------------------*)
(* Pure formulas                                *)
(*----------------------------------------------*)
module SHpure = struct
(* 'pp' is used *)

  type t = SHpureExp.t list

  let fv = map_union SHpureExp.fv

  let lab = map_union SHpureExp.lab

  let to_string (pp : t) =
    string_of_list SHpureExp.to_string " & " pp

  let print (pp : t) = print_string (to_string pp)
    
  let println (pp : t) = print_endline (to_string pp)

  let rename (rr : (string* string) list) =
    List.map (SHpureExp.rename rr)
    
  let subst (sub : (string * SHterm.t) list) =
    List.map (SHpureExp.subst sub)

  let rec simpl (pp : t) =
    match pp with
    | [] -> []
    | SHpureExp.Eq(t1,t2)::rest  ->
       if SHterm.compare t1 t2 = 0
       then simpl rest
       else SHpureExp.Eq(t1,t2)::(simpl rest)
    | p::rest ->
       p::(simpl rest)

  let rec ccp (pp : t) =
    match pp with
    | [] -> true
    | p::rest -> SHpureExp.ccp && ccp rest

  let rec minusL (pp : t) =
    match pp with
    | [] -> []
    | At(a,s)::rest -> minusL rest
    | p::rest -> p::(minusL rest)
end

(* Short cuts for SHpures *)
let ( =..= ) (ts1 : SHterm.t list) (ts2 : SHterm.t list) : SHpure.t = 
  let zipts = zipLst ts1 ts2 in
  List.map (fun (x,y) -> x =.= y) zipts

(*----------------------------------------------*)
(* Symbolic Heaps                               *)
(*----------------------------------------------*)
module SH = struct
  (* h is used *)
  type t = SHpure.t * SHspat.t

  let fv (pure,spat) = union (SHpure.fv pure) (SHspat.fv spat)

  let lab (pure,spat) = union (SHpure.lab pure) (SHspat.lab spat)

  let to_string (pure,spat) =
	let piPart = "pi:[" ^ (SHpure.to_string pure) ^ "], " in
	let sgPart = "sg:[" ^ (SHspat.to_string spat) ^ "]" in
	piPart ^ sgPart

  let print (h : t) = print_string (to_string h)
    
  let println (h : t) = print_string ((to_string h)^"\n")

  let rename (rr : (string* string) list) (pure, spat) =
    (SHpure.rename rr pure, SHspat.rename rr spat)
    
  let subst (sub : (string * SHterm.t) list) (pure, spat) =
    (SHpure.simpl (SHpure.subst sub pure), SHspat.subst sub spat)

  let add_pureexp (h : t) (t : SHpureExp.t) =
    match h with
    | (p,s) -> (t::p,s)

  let ccp (h : t) =
    match h with
    | (pure,spat) = SHpure.ccp pure && SHspat.ccp spat

  let root (h : t) =
    match h with
    | (pure,spat) = ["x"] (* todo *)
end



(*----------------------------------------------*)
(* Single-conclusion Entailments                 *)
(*----------------------------------------------*)
module Entl = struct
  type t =
	{
	  mutable up  : string list;
	  mutable ant : SH.t;
	  mutable suc : SH.t;
	}

  let create up ant suc = { up = up; ant = ant; suc = suc }
  
  let fv e = union (SH.fv e.ant) (SH.fv e.suc)

  let lab e = union (SH.lab e.ant) (SH.lab e.suc)

  let to_string (e : t) =
    let up = string_of_list (fun s -> s) "," e.up in
    let shant = SH.to_string e.ant in
    let shsuc = SH.to_string e.suc in
	"up(" ^ up ^ "):\n" ^ shant ^ "\n|-\n" ^ shsuc

  let print (e : t) = print_string (to_string e)

  let println (e : t) = print_string ((to_string e)^"\n")

  let rename (rr : (string* string) list) e =
    { up = e.up; ant = SH.rename rr e.ant; suc = SH.rename rr e.suc }
    
  let subst (sub : (string * SHterm.t) list) e =
    { up = e.up; ant = SH.subst sub e.ant; suc = SH.subst sub e.suc }

  let add_pureexp (e : t) (t : SHpureExp.t) =
    { up = e.up; ant = SH.add_pureexp e.ant t; suc = e.suc }

  let ccp (e : t) =
    SH.ccp e.ant && SH.ccp e.suc
end
