(*
#use "loader.ml";;
 *)
open Tools;;

(*------------------------------------------------------*)
(* Terms of Symbolic Heap			*)
(*------------------------------------------------------*)
module SHterm = struct

type t = 
  | Var of string
  | Nil

let fv tm = match tm with
  | Var v -> [v]
  | _ -> []

let to_string tm = match tm with
  | Var v -> v
  | Nil -> "nil"

let print (it : t) = print_string ((to_string it)^"\n")

let extractVars (tmL : t list) = 
  let rec extVarsR res tL = 
  match tL with
  | [] -> List.rev res
  | hd::tL' -> 
	match hd with
	| Var x -> extVarsR (x::res) tL'
	| _ -> extVarsR res tL'
  in extVarsR [] tmL

(* Nil > z > y > x *)
let order tm1 tm2 = 
match (tm1,tm2) with
  | (Nil,Nil) -> 0
  | (Nil,_) -> 1
  | (Var x,Var y) -> strlexorder x y
  | (_,_) -> -1

(* Substitution on terms *)
let  varRepl (repl : (string* string) list) tm = 
match tm with
  | Var v -> 
     begin 
	match findItemOption v repl with
	| Some w -> Var w
	| None -> tm
     end
  | _ -> tm

let rec subst (sub : (string * t) list) tm = match sub with
  | [] -> tm
  | (v,tm1)::rest -> if tm = Var v then tm1 else subst rest tm

end;;

module Subst = struct 
type t = (string * SHterm.t) list
end;;

(*------------------------------------------------------*)
(* A Pure Expression of Symbolic Heap	*)
(*------------------------------------------------------*)
module SHpureExp = struct 

type t = 
  | Eq of SHterm.t * SHterm.t
  | Neq of SHterm.t * SHterm.t

let fv t = match t with
  | Eq(t1,t2) -> 
	let varL1 = SHterm.fv t1 in
	let varL2 = SHterm.fv t2 in
	List.append varL1 varL2
  | Neq(t1,t2) -> 
	let varL1 = SHterm.fv t1 in
	let varL2 = SHterm.fv t2 in
	List.append varL1 varL2

let to_string t = match t with
  | Eq(t1,t2) -> (SHterm.to_string t1)^" = "^(SHterm.to_string t2)
  | Neq(t1,t2) -> (SHterm.to_string t1)^" <> "^(SHterm.to_string t2)

let print (it : t) = print_string ((to_string it)^"\n")

let varRepl repl pr = 
match pr with
  | Eq(tm1,tm2) -> 
	let tm1' = SHterm.varRepl repl tm1 in
	let tm2' = SHterm.varRepl repl tm2 in
	Eq(tm1',tm2')
  | Neq(tm1,tm2) -> 
	let tm1' = SHterm.varRepl repl tm1 in
	let tm2' = SHterm.varRepl repl tm2 in
	Neq(tm1',tm2')

let subst (sub : Subst.t) ex = match ex with
  | Eq(tm1,tm2) -> 
	let tm1' = SHterm.subst sub tm1 in
	let tm2' = SHterm.subst sub tm2 in
	Eq(tm1',tm2')
  | Neq(tm1,tm2) -> 
	let tm1' = SHterm.subst sub tm1 in
	let tm2' = SHterm.subst sub tm2 in
	Neq(tm1',tm2')

let nf p = match p with
  | Eq(tm1,tm2) ->
     let comp = SHterm.order tm1 tm2 in
     let tm1' = if comp > 0 then tm2 else tm1 in
     let tm2' = if comp > 0 then tm1 else tm2 in
     Eq(tm1',tm2')
  | Neq(tm1,tm2) ->
     let comp = SHterm.order tm1 tm2 in
     let tm1' = if comp > 0 then tm2 else tm1 in
     let tm2' = if comp > 0 then tm1 else tm2 in
     Neq(tm1',tm2')

(* Neq(y,z) > Neq(a,b) > Eq(y,z) > Eq(a,b) *)
(* p1 and p2 are assumed to be normal form *)
let order p1 p2 = 
  let module T = SHterm in
  match p1,p2 with
  | Neq(t1a,t1b),Neq(t2a,t2b) -> 
     if t1a = t2a then T.order t1b t2b else T.order t1a t2a
  | Eq(t1a,t1b),Eq(t2a,t2b) -> 
     if t1a = t2a then T.order t1b t2b else T.order t1a t2a
  | Neq(_,_), Eq(_,_) -> 1
  | Eq(_,_), Neq(_,_) -> -1

end;;

(*------------------------------------------------------*)
(* Pure part of Symbolic Heap		*)
(*------------------------------------------------------*)
module SHpure = struct		

  type t = SHpureExp.t list

  let fv (pure : t) = List.concat (List.map SHpureExp.fv pure)

  let to_string (it : t) = 
	string_of_list (fun s->s) " & " (List.map SHpureExp.to_string it)

  let print (it : t) = print_string ((to_string it)^"\n")

  let varRepl repl (pure : t) : t = 
	List.map (SHpureExp.varRepl repl) pure

  let split (pure : t) : t * t =
    let module P = SHpureExp in
    let rec splitRec pu (res1,res2) = 
      match pu with	
      | [] -> (List.rev res1,List.rev res2)
      | p::pu' ->	 
	 match p with	   
	 | P.Eq(_,_) -> splitRec pu' (p::res1,res2)
	 | P.Neq(_,_) -> splitRec pu' (res1,p::res2)
    in splitRec pure ([],[])
		 
  let subst sub (pure : t) : t = 
	List.map (SHpureExp.subst sub) pure

  let sort (pure : t) : t =
    let module P = SHpureExp in
    let pure' = List.map P.nf pure in
    List.fast_sort P.order pure'

end;;

module SHspatExp = struct

type t = 
  | Emp
  | Alloc of SHterm.t * SHterm.t list
  | Ind of string * SHterm.t list

let fv e = match e with
  | Emp -> []
  | Alloc(tm,tmL) -> 
	let tmL1 = tm::tmL in 
	List.concat (List.map SHterm.fv tmL1)
  | Ind(_,tmL) -> 
	List.concat (List.map SHterm.fv tmL);;

let to_string it = match it with
  | Emp -> "Emp"
  | Alloc(tm,tmL) -> 
	let tms = SHterm.to_string tm in
	let tmLs = concatStrLComma (List.map SHterm.to_string tmL) in
	tms^" -> ("^tmLs^")"
  | Ind(pr,tmL) -> 
	let tmLs = concatStrLComma (List.map SHterm.to_string tmL) in
	if tmL = [] then pr else pr^"("^tmLs^")"

let print (it : t) = print_string ((to_string it)^"\n")

let allocTerms (sp : t) = match sp with
  | Alloc(tm,_) -> [tm]
  | _ -> []

let refTerms (sp: t) = match sp with
  | Emp -> []
  | Ind(_,tmL) -> []
  | Alloc(_,tmL) -> [tmL]

let indpred (sp : t) = match sp with
  | Emp -> []
  | Alloc(_,_) -> []
  | Ind(pr,tmL) -> [(pr,tmL)]

let varRepl repl (sp : t) : t = match sp with
  | Emp -> Emp
  | Alloc(tm,tmL) -> 
	let tm' = SHterm.varRepl repl tm in
	let tmL' = List.map (SHterm.varRepl repl) tmL in
	Alloc(tm',tmL')
  | Ind(pr,tmL) ->
	let tmL' = List.map (SHterm.varRepl repl) tmL in
	Ind(pr,tmL')

let subst sub ex = match ex with
  | Emp -> Emp
  | Alloc(tm1,tmL) -> 
	let tm1' = SHterm.subst sub tm1 in
	let tmL' = List.map (SHterm.subst sub) tmL in
	Alloc(tm1',tmL')
  | Ind(pr,tmL) -> 
	let tmL' = List.map (SHterm.subst sub) tmL in
	Ind(pr,tmL')

let weakorder s1 s2 = match s1,s2 with
  | Alloc(t1,_),Alloc(t2,_) -> SHterm.order t1 t2
  | _,_ -> 0

(* P2(x) > P1(y) > P1(x) > y->(..) > x->(..) > Emp *)	     
let order s1 s2 = match s1,s2 with
  | Emp,_ -> if s2 = Emp then 0 else -1
  | Alloc(tm1,_),Emp -> 1
  | Alloc(tm1,tmL1),Alloc(tm2,tmL2) ->
     if tm1 = tm2 then lexorder SHterm.order tmL1 tmL2
     else SHterm.order tm1 tm2
  | Alloc(_,_),_ -> -1
  | Ind(pr1,tmL1),Ind(pr2,tmL2) ->
     if pr1 = pr2 then lexorder SHterm.order tmL1 tmL2
     else strlexorder pr1 pr2
  | Ind(pr,tm1),_ -> 1
	   
end;;

module SHspat = struct

type t = SHspatExp.t list

let fv (pr : t) = List.concat (List.map SHspatExp.fv pr)

let to_string (sp : t) =
  string_of_list (fun s->s) " * " (List.map SHspatExp.to_string sp)

let print it = print_string ((to_string it)^"\n")

let split (spat : t) =
  let module S = SHspatExp in
  let rec splitRec allc ind sp = match sp with
    | [] -> (List.rev allc,List.rev ind)
    | hd::sp' ->
       match hd with
       | S.Emp -> splitRec allc ind sp'
       | S.Alloc(_,_) -> splitRec (hd::allc) ind sp'
       | S.Ind(_,_) -> splitRec allc (hd::ind) sp'
  in splitRec [] [] spat
	       
let allocTerms (spat : t) = 
  List.concat (List.map SHspatExp.allocTerms spat);;

let refTerms (spat : t) = 
 List.concat (List.map SHspatExp.refTerms spat)

let indpred (spat : t) = 
  let ind3 = List.concat (List.map SHspatExp.indpred spat) in
  let prL = dropRed (List.map (fun (x,_) -> x) ind3) in
  let gather1 pr (pr1,tmL1) = if pr = pr1 then [tmL1] else [] in
  let gather pr = (pr, List.concat (List.map (gather1 pr) ind3)) in
  List.map gather prL

let varRepl repl (spat : t) : t = 
	List.map (SHspatExp.varRepl repl) spat

let subst (sub : Subst.t) (spat : t) : t = 
	List.map (SHspatExp.subst sub) spat

let sort (spat : t) : t =
  let module S = SHspatExp in
  List.sort S.order spat

end;;

(* symbolic heaps of Brotherston's paper *)
module SH = struct
(* (pure,spat) means pure & spat *)
  type t = SHpure.t * SHspat.t 

  let to_string (it : t) = 
    let (pure,spat) = it in
    let pureStr = SHpure.to_string pure in
    let spatStr = SHspat.to_string spat in
    match pure,spat with
    | [],[] -> ""
    | _,[] -> pureStr
    | [],_ -> spatStr
    | _,_ -> pureStr^" & "^spatStr

  let print (it : t) = print_string (to_string it)

  let println (it : t) = print_endline (to_string it)                     

  let allvars (sh : t) =
    let (pure,spat) = sh in
    let fvp = SHpure.fv pure in
    let fvs = SHspat.fv spat in
    List.rev_append fvp fvs

  let bv prm (sh : t) =
    let vars = List.fast_sort strlexorder (allvars sh) in
    let vars' = dropRedSorted (=) vars in
    let prm' = List.fast_sort strlexorder prm in
    List_tailrec.dropsame strlexorder vars' prm'
		    
  let subst (sub : Subst.t) (sh : t) =
    let (pure,spat) = sh in
    let pure' = SHpure.subst sub pure in
    let spat' = SHspat.subst sub spat in
    (pure',spat')
    
  let nf (it : t) : t =
    let (pure,spat) = it in
    let pure' = SHpure.sort pure in
    (pure',spat)
    
end;;

  
module IS = struct 
(* ("P",[x;y],[def1;def2]) means P(x,y) := def1 | def2 *)
  type t = (string * string list * SH.t list) list

  let preds (is : t) = List.map (fun (p,_,_)->p) is
	
  let rule_of (is : t) pr = 
    match List.filter (fun (p,_,_) -> p = pr) is with
    | [] -> None
    | (_,prm,defL)::_ -> Some (pr,prm,defL)

  let nthRule (is : t) pr n = 
    match rule_of is pr with
    | None -> None
    | Some(_,prm,defL) -> 
	if List.length defL <= n then None else Some (pr,prm,List.nth defL n)

  let to_string1 (pr,prms,defL) = 
    let prmStr = concatStrLComma prms in 
    let head = if prms = [] then pr else pr^"("^prmStr^")" in
    let defStrL = List.map SH.to_string defL in
    let defStr = List.fold_right (fun x->fun y-> if y = "" then x else x^" | "^y) defStrL "" in
    head^" := "^defStr

  let to_string (is : t) = 
    let isStrL = List.map to_string1 is in
    List.fold_right (fun x->fun y-> if y = "" then x else x^" \n\n"^y) isStrL ""

  let print (is : t) = print_string ((to_string is)^"\n")

  let prmconv (pr,prms,defL) =
    let len = List.length prms in
    let numL = List.rev (genLst len) in
    let prms' = List.map (fun n -> "%"^(string_of_int n)) numL in
    let sub = zipLst prms (List.map (fun v -> SHterm.Var v) prms') in
    let defL' = List.map (fun (p,s) -> (SHpure.subst sub p,SHspat.subst sub s)) defL in
    (pr,prms',defL')

  let spatconv (pr,prms,defL) =
    let defL' = List.map SH.nf defL in
    (pr,prms,defL')
    
  let nf (is : t) : t = List.map (fun def -> spatconv(prmconv def)) is

end;;

module Entl = struct
  type t = SH.t * SH.t
		    
  let to_string (entl : t) =
    let (sh1,sh2) = entl in
    let shstr1 = SH.to_string sh1 in
    let shstr2 = SH.to_string sh2 in
    shstr1^" |- "^shstr2

  let print (entl : t) = print_string ((to_string entl)^"\n")

end;;
