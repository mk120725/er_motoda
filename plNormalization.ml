open Tools;;
open Ratio;;
open NewSyntax;;

(* n_E(Sigma) *)

let rec norm_E (spat : SHspat.t) : string list =
  match spat with
  | Emp -> []
  | SAtom a -> []
  | SCon (s1,s2) ->
     Tools.unionLst
       (Tools.unionLst
          (Tools.interLst (SHspat.lab s1) (SHspat.lab s2))
          (norm_E s1))
       (norm_E s2)
  | WCon (s1,s2) ->
     Tools.unionLst (norm_E s1) (norm_E s2)

let rec check_pto (spat : SHspat.t) : bool =
  match spat with
  | Emp -> false
  | SAtom at ->
     (match at with
      | Pto(t,ts) -> true
      | _ -> false)
  | SCon(s1,s2) -> (check_pto s1) || (check_pto s2)
  | _ -> false

let rec ls_to_pure (spat : SHspat.t) : SHpure.t =
  match spat with
  | Emp -> []
  | SAtom(Pr(p,t1::t2::[])) ->
     if p = "ls" then [Eq(t1,t2)] else []
  | SAtom(_) -> []
  | SCon(s1,s2) -> (ls_to_pure s1) @ (ls_to_pure s2)
  | _ -> []

(* n_p(Pi,L) *)
let rec norm_p (pure : SHpure.t) (labs : string list) : SHpure.t =
  match pure with
  | [] -> []
  | pa::rest ->
     match pa with
     | Eq(t1,t2) -> Eq(t1,t2)::(norm_p rest labs)
     | NEq(t1,t2) -> NEq(t1,t2)::(norm_p rest labs)
     | At(a,spat) ->
        if (List.mem a labs)
        then 
          if (check_pto spat)
          then
            NEq(nil,nil)::At(a,Emp)::[]
          else
            let pure = ls_to_pure spat in
            At(a,Emp)::pure
        else At(a,spat)::(norm_p rest labs)

(* extract_labels s1n labels -> (spat1, spat2)
      s1n = spatial formulas without WCon
      labels = list of labels
      spat1 = label part of s1n (list)
      spat2 = rest of s1n
 *)

let rec extract_labels (spat : SHspat.t) (labs : string list) : SHspatExp.t list * SHspat.t=
  match spat with
  | Emp -> ([],Emp)
  | SAtom s -> (
    match s with
    | Lab (a,p) -> if List.mem a labs then ([s],Emp) else ([],spat)
    | _ -> ([],spat)
  )
  | SCon(s1,s2) ->
     let (s1l,s1r) = extract_labels s1 labs in
     let (s2l,s2r) = extract_labels s2 labs in
     (s1l @ s2l, SCon(s1r,s2r))
  | WCon(s1,s2) -> ([],Emp)

let rec check_lab a (s : SHspatExp.t) =
  match s with
  | Lab (b,_) -> a = b
  | _ -> false

(* findOption : (SHspatExp -> bool) -> SHspatExp list -> SHspatExp option
   check_lab : string -> SHspatExp -> bool
   check_lab a : SHspatExp -> bool *)

let rec add_permission (l1 : SHspatExp.t list) (l2 : SHspatExp.t list) : SHspat.t =
  match l1 with
  | [] -> Emp
  | (Lab (a,p1)) :: rest ->
     (match Tools.findOption (check_lab a) l2 with
     | Some (Lab (a,p2)) ->
         SCon(SAtom(Lab (a, (p1 +/ p2))), add_permission rest l2)
     | _ -> Emp)
  | _ -> Emp

(* n_s(Sigma) *)  
let rec norm_s (spat : SHspat.t) : SHspat.t =
  match spat with
  | Emp -> Emp
  | SAtom s -> SAtom s
  | WCon(s1, s2) ->
     let s1n = norm_s s1 in
     let s2n = norm_s s2 in
     let commonlabels =
       Tools.interLst (SHspat.lab s1n) (SHspat.lab s2n) in
     let (s1l,s1r) = extract_labels s1n commonlabels in
     let (s2l,s2r) = extract_labels s2n commonlabels in
     let spat1 = add_permission s1l s2l in
     SCon(spat1, SCon(s1r, s2r))
  | SCon(s1, s2) -> WCon(s1, s2)
     

let normalization (phi : SH.t) : SH.t =
  let (pure, spat) = phi in
  let nf_pure = norm_p pure (norm_E spat) in
  let nf_spat = norm_s spat in
  (nf_pure, nf_spat)


    
