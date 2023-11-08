open Tools;;
open Ratio;;
open NewSyntax;;

(* n_E(Sigma) *)
let rec norm_E (spat : newSyntax.SHspat.t) : string list =
  match spat with
  | Emp -> []
  | SAtom a -> []
  | SCon (s1,s2) ->
     Tools.unionLst
       (Tools.unionLst
          (Tools.interLst (newSyntax.SHspat.lab s1) (newSyntax.SHspat.lab s2))
          (norm_E s1))
       (norm_E s2)
  | WCon (s1,s2) ->
     Tools.unionLst (norm_E s1) (norm_E s2)

(* n_p(Pi,L) *)
let norm_p (pure : newSyntax.SHpure.t) (labs : string list) : newSyntax.SHpure.t =
  match pure with
  | [] -> []
  | pa::rest ->
     match pa with
     | Eq(t1,t2) -> Eq(t1,t2)::(norm_p rest labs)
     | NEq(t1,t2) -> NEq(t1,t2)::(norm_p rest labs)
     | At(a,spat) ->
        if (List.mem a labs)
        then 
          if (Emp <> )
          Nil <> Nil 
          else 
        else At(a,spat)::(norm_p rest labs)

(* extract_labels s1n labels -> (spat1, spat2)
      s1n = spatial formulas without WCon
      labels = list of labels
      spat1 = label part of s1n (list)
      spat2 = rest of s1n
 *)

let rec extract_labels (spat : newSyntax.SHspat.t) (labs : string list) =
  match spat with
  | Emp -> ([],Emp)
  | SAtom s -> (
    match s with
    | Lab a p -> if List.mem a labs then ([s],Emp) else ([],spat)
    | _ -> (Emp,spat)
  )
  | SCon(s1,s2) ->
     let (s1l,s1r) = extract_labels s1 labs in
     let (s2l,s2r) = extract_labels s2 labs in
     (s1l @ s2l, SCon(s1r,s2r))
  | WCon(s1,s2) -> ([],Emp)

let rec add_permission (l1 : SHspatExp list) (l2 : SHspatExp list) =
  match l1 with
  | [] -> Emp
  | (Lab a p1) :: rest ->
    let Lab a p2 = Tools.findItemOption (Lab a _) l2 in
     Scon(Lab a (Ratio.+/ p1 p2), add_permission rest l2)
  | _ -> Emp

(* n_s(Sigma) *)  
let rec norm_s (spat : newSyntax.SHspat.t) : newSyntax.SHspat.t =
  match spat with
  | Emp -> Emp
  | SAtom s -> SAtom s
  | WCon(s1, s2) ->
     let s1n = norm_s s1 in
     let s2n = norm_s s2 in
     let commonlabels =
       Tools.interLst (newSyntax.SHspat.lab s1n) (newSyntax.SHspat.lab s2n) in
     let (s1l,s1r) = extract_lables s1n commonlabels in
     let (s2l,s2r) = extract_lables s2n commonlabels in
     let spat1 = add_permission s1l s2l in
     SCon(spat1, SCon(s1r, s2r))
  | Scon(s1, s2) -> Wcon(s1, s2)
     

let normalization (phi : newSyntax.SH.t) : newSyntax.SH.t =
  let (pure, spat) = phi in
  let nf_pure = norm_p pure (norm_E spat) in
  let nf_spat = norm_s spat in
  (nf_pure, nf_spat)


    
