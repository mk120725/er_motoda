open NewSyntax;;
open CcSyntax;;

(* translator from new to cc *)
let new2cc_term (term : NewSyntax.SHterm.t) : CcSyntax.SHterm.t =
  match term with
  | Var s -> CcSyntax.var s
  | Nil -> CcSyntax.nil

let new2cc_termlist = List.map new2cc_term

let new2cc_spatatom
      (satom : NewSyntax.SHspatExp.t)
      (dict : (string * CcSyntax.SHgamma.t) list) : CcSyntax.SHgamma.t =
  match satom with
  | Emp -> [(CcSyntax.SHspatExp.Emp,[])]
  | Pto(t,ts) -> [(CcSyntax.SHspatExp.Pto(new2cc_term t,new2cc_termlist ts),[])]
  | Pr(pr,ts) -> [(CcSyntax.SHspatExp.Pr(pr,new2cc_termlist ts),[])]
  | Lab(a,p) ->
     (match Tools.findItemOption a dict with
     | None -> [(CcSyntax.SHspatExp.Emp,[])]
     | Some spat -> spat)

let rec new2cc_spat
          (spat : NewSyntax.SHspat.t)
          (dict : (string * CcSyntax.SHgamma.t) list) : CcSyntax.SHgamma.t =
  match spat with
  | Emp -> []
  | SAtom(a) -> new2cc_spatatom a dict
  | SCon(s1,s2) -> (new2cc_spat s1 dict) @ (new2cc_spat s2 dict)
  | WCon(s1,s2) -> (new2cc_spat s1 dict) @ (new2cc_spat s2 dict)

let new2cc_pureatom (patom : NewSyntax.SHpureExp.t) : CcSyntax.SHpure.t =
  match patom with
  | Eq(t1,t2) -> [(CcSyntax.Eq, new2cc_term t1, new2cc_term t2)]
  | NEq(t1,t2) -> [(CcSyntax.Neq, new2cc_term t1, new2cc_term t2)]
  | At(s,t) -> []

let rec new2cc_pure (pure : NewSyntax.SHpure.t) =
  match pure with
  | [] -> []
  | patom::rest -> (new2cc_pureatom patom) @ (new2cc_pure rest)

let rec ext_at (pure : NewSyntax.SHpure.t) =
  match pure with
  | [] -> []
  | At(s,spat)::rest -> (s,new2cc_spat spat [])::(ext_at rest)
  | _::rest -> ext_at rest

let new2cc_entl (entl : NewSyntax.Entl.t) : CcSyntax.Entl.t =
  let up = entl.up in
  let (newpure_l, newspat_l) = entl.ant in
  let dict_l = ext_at newpure_l in
  let (newpure_r, newspat_r) = entl.suc in
  let dict_r = ext_at newpure_r in
  let ccant = CcSyntax.SH.create [] (List.map CcSyntax.var up) []
                (new2cc_pure newpure_l)
                (new2cc_spat newspat_l dict_l) in
  let ccsuc = [CcSyntax.SH.create [] [] []
                (new2cc_pure newpure_r)
                (new2cc_spat newspat_r dict_r)
              ] in
  { name = ""; ant = ccant; suc = ccsuc }
