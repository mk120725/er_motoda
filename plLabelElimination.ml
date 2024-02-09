open Tools;;
open NewSyntax;;
open Ratio;;

let rec extract_label_pure (pure1 : NewSyntax.SHpure.t) (s : string) (pure2 : NewSyntax.SHpure.t) =
  match pure1 with 
  | [] -> pure2
  | At(a,spat)::rest ->
     if (a = s) then (pure2 @ rest) else (extract_label_pure rest s (At(a,spat)::pure2))
  | p::rest -> extract_label_pure rest s (p::pure2)

let rec extract_label_spat1 (pure1 : NewSyntax.SHpure.t) (s : string) =
  match pure1 with 
  | [] -> NewSyntax.SHspat.Emp
  | At(a,spat)::rest ->
     if (a = s) then spat else (extract_label_spat1 rest s)
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

let rec lab_elim (entl : NewSyntax.Entl.t) : NewSyntax.Entl.t list option =
  let lab_ant = NewSyntax.SH.lab entl.ant in
  let lab_suc = NewSyntax.SH.lab entl.suc in
  (* print_string "---lab_ant---\n";
  Tools.print_labels lab_ant; *)
  if (Tools.unionLst lab_ant lab_suc = [])
  then
    Some [entl]
  else if (Tools.elimElemLstL lab_ant lab_suc <> [])
  then
    None
  
  else if (Tools.elimElemLstL lab_suc lab_ant <> [])
  then
    match Tools.elimElemLstL lab_suc lab_ant with
    | [] -> None
    | s::rest ->
       let (spat1_ant,pure1_ant,p_ant,spat2_ant) = extract_label entl.ant s in
       if (p_ant </ Ratio.make_ratio 1 1)
       then
         if (NewSyntax.SHspat.empp spat1_ant)
         then
           lab_elims [NewSyntax.Entl.create entl.up (pure1_ant,spat2_ant) entl.suc ]
         else
           None
       else
         lab_elims [NewSyntax.Entl.create entl.up (pure1_ant,SCon(spat1_ant,spat2_ant)) entl.suc ]
  else 
    let lab_inter = Tools.interLst lab_ant lab_suc in 
    match lab_inter with
    | [] -> None
    | s::rest ->
      let (spat1_ant,pure1_ant,p_ant,spat2_ant) = extract_label entl.ant s in
       print_string "---extract_label---\n";
       (* NewSyntax.SH.println entl.ant;
       print_string ("label: " ^ s ^ "\n");
       NewSyntax.SHspat.println spat1_ant;
       NewSyntax.SHpure.println pure1_ant;
       NewSyntax.SHspat.println spat2_ant;*)
      let (spat1_suc,pure1_suc,p_suc,spat2_suc) = extract_label entl.suc s in
      if (p_ant =/ p_suc)
      then
        lab_elims [NewSyntax.Entl.create (Tools.unionLst entl.up (NewSyntax.SH.root (pure1_ant, spat2_ant))) 
                    (NewSyntax.SHpure.minusL pure1_ant,spat1_ant)
                    (NewSyntax.SHpure.minusL pure1_suc,spat1_suc);
                  NewSyntax.Entl.create (Tools.unionLst entl.up (NewSyntax.SH.root ([], spat1_ant))) 
                    (pure1_ant,spat2_ant)
                    (pure1_suc,spat2_suc)]
      else if (NewSyntax.SHspat.empp spat1_ant)
      then 
        lab_elims [NewSyntax.Entl.create (Tools.unionLst entl.up (NewSyntax.SH.root (pure1_ant, spat2_ant)))
                    (NewSyntax.SHpure.minusL pure1_ant,NewSyntax.SHspat.Emp)
                    (NewSyntax.SHpure.minusL pure1_suc,spat1_suc);
                  NewSyntax.Entl.create entl.up
                    (pure1_ant,spat2_ant)
                    (pure1_suc,spat2_suc)]
      else
        None

and lab_elims (entls : NewSyntax.Entl.t list) : NewSyntax.Entl.t list option = 
  match entls with
  | [] -> Some []
  | t::rest ->
    match lab_elim t with
    | None -> None
    | Some es1 ->
      (match lab_elims rest with
      | None -> None
      | Some es2 -> Some (es1 @ es2))

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
