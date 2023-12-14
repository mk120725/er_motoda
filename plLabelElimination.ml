open Tools;;
open NewSyntax;;
open Ratio;;

let rec get_permission (t : NewSyntax.SH.t) (ts : string list) =
  match ts with
  | [] -> []
  | t1::rest -> 

let rec lab_elim (etnls : NewSyntax.Entl.t list) : NewSyntax.Entl.t list = 
  match entls with
  | [] -> []
  | t::rest ->
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
      (*e11*)::(*e12*)::(lab_elim rest)
