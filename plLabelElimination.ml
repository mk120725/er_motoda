open Tools;;
open NewSyntax;;
open Ratio;;

let rec extract_label_pure (pure1 : NewSyntax.SHpure.t) (s : string) (pure2 : NewSyntax.SHpure.t) =
  match pure1 with 
  | [] -> pure2
  | At(a,spat)::rest -> pure2 @ rest
  | p::rest -> extract_label_pure rest s (p::pure2)

let rec extract_label_spat1 (pure1 : NewSyntax.SHpure.t) (s : string) =
  match pure1 with 
  | [] -> NewSyntax.SHspat.Emp
  | At(a,spat)::rest -> spat
  | p::rest -> extract_label_spat1 rest s 

let rec extract_label_perm (spat : NewSyntax.SHspat.t) (s : string) =
  match spat with 
  | Emp -> Ratio.make_ratio 1 1
  | SAtom(Lab(a,p)) -> 
    if (a = s)
    then
      p
    else 
      Ratio.make_ratio 1 1
  | SAtom(_) -> Ratio.make_ratio 1 1
  | SCon(sp1,sp2) ->
    (extract_label_perm sp1 s) */ (extract_label_perm sp2 s)
  | WCon(sp1,sp2) ->
    (extract_label_perm sp1 s) */ (extract_label_perm sp2 s)

let rec extract_label_spat2 (spat : NewSyntax.SHspat.t) (s : string) =
  match spat with 
  | Emp -> NewSyntax.SHspat.Emp
  | SAtom(Lab(a,p)) -> 
    if (a = s)
    then
      NewSyntax.SHspat.Emp
    else 
      spat
  | SAtom(_) -> spat
  | SCon(sp1,sp2) ->
    SCon((extract_label_spat2 sp1 s),(extract_label_spat2 sp2 s))
  | WCon(sp1,sp2) ->
    NewSyntax.SHspat.Emp

let extract_label (t : NewSyntax.SH.t) (s : string) =
  match t with
  | (pure,spat) -> 
    let pure1 = extract_label_pure pure s [] in
    let spat1 = extract_label_spat1 pure s in
    let p = extract_label_perm spat s in
    let spat2 = extract_label_spat2 spat s in
    (spat1,pure1,p,spat2) 

let lab_elim (etnl : NewSyntax.Entl.t) : NewSyntax.Entl.t list option =
  let lab_ant = NewSyntax.SH.lab entl.ant in
  let lab_suc = NewSyntax.SH.lab entl.suc in
  if (Tools.unionLst lab_ant lab_suc = [])
  then
    Some [entl]
  if (Tools.elimElemLstL lab_ant lab_suc <> [])
  then
    None
    
  let lab_inter = Tools.interLst lab_ant lab_suc in 
  match lab_inter with
  | [] -> (*lab_ant だけNoEmptyの場合*)
  | s::rest ->
    let (spat1_ant,pure1_ant,p_ant,spat2_ant) = extract_label entl.ant s in
    let (spat1_suc,pure1_suc,p_suc,spat2_suc) = extract_label entl.suc s in

    if (p_ant =/ p_suc)
    then
      (*e11*)::(*e12*)
    else
      None
  
  if (Tools.elimElemLstL lab_suc lab_ant <> [])
  then
    if (p_ant </ Ratio.make_ratio 1 1 & )
    None


let rec lab_elims (etnls : NewSyntax.Entl.t list) : NewSyntax.Entl.t list = 
  match entls with
  | [] -> []
  | t::rest ->
    (lab_elim t)::lab_elims rest 

  (*
    let lab_ant = NewSyntax.SH.lab t.ant in
    let lab_suc = NewSyntax.SH.lab t.suc in
    if (Tools.unionLst lab_ant lab_suc = [])
    then
      t::(lab_elim rest)
    if (Tools.elimElemLstL lab_ant lab_suc <> [])
    then
      (*invalid*)
    
    let lab_inter = Tools.interLst lab_ant lab_suc in 
    if (lab_inter <> [])
    then
      get_permisson 

      if ((*pi=delta*))
      then

      else
        (*invalid*)
    if (Tools.elimElemLstL lab_suc lab_ant <> [])
    then
      (*e11*)::(*e12*)::(lab_elims rest)
  *)
