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
        then ...
        else At(a,spat)::(norm_p rest labs)

  
(* n_s(Sigma) *)  
let rec norm_s (spat : newSyntax.SHspat.t) : newSyntax.SHspat.t =
  match spat with
  | SAtom a -> a
  | Emp -> Emp
  | Wcon (s1,s2) -> 
  | Scon (s1,s2) -> norm_s (Wcon (s1,s2))
    


let normalization (phi : newSyntax.SH.t) : newSyntax.SH.t =
  let (pure, spat) = phi in
  let nf_pure = norm_p pure (norm_E spat) in
  let nf_spat = norm_s spat in
  (nf_pure, nf_spat)


    
