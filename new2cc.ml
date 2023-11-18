open NewSyntax;;
open CcSyntax;;

(* translator from new to cc *)
let new2cc_term (term : NewSyntax.SHterm.t) : CcSyntax.SHterm.t =
  match term with
  | Var s -> CcSyntax.var s
  | Nil -> CcSyntax.nil

let new2cc_termlist = List.map new2cc_term

let new2cc_spatatom (satom : NewSyntax.SHspatExp.t) : CcSyntax.SHgammaExp.t =
  match satom with
  | Emp -> (CcSyntax.SHspatExp.Emp,[])
  | Pto(t,ts) -> (CcSyntax.SHspatExp.Pto(new2cc_term t,new2cc_termlist ts),[])
  | Pr(pr,ts) -> (CcSyntax.SHspatExp.Pr(pr,new2cc_termlist ts),[])
  | Lab(a,p) -> (CcSyntax.SHspatExp.Emp,[])

let rec new2cc_spat (spat : NewSyntax.SHspat.t) : CcSyntax.SHgamma.t =
  match spat with
  | Emp -> []
  | SAtom(a) -> [new2cc_spatatom a]
  | SCon(s1,s2) -> (new2cc_spat s1) @ (new2cc_spat s2)
  | WCon(s1,s2) -> (new2cc_spat s1) @ (new2cc_spat s2)

let new2cc_pureatom (patom : NewSyntax.SHpureExp.t) : CcSyntax.SHpureExp.t =
  match patom with
  | Eq(t1,t2) -> (CcSyntax.Eq, new2cc_term t1, new2cc_term t2)
  | NEq(t1,t2) -> (CcSyntax.Neq, new2cc_term t1, new2cc_term t2)
  | At(s,t) -> (CcSyntax.Eq, CcSyntax.nil, CcSyntax.nil)

let new2cc_pure = List.map new2cc_pureatom

let new2cc_entl (entl : NewSyntax.Entl.t) : CcSyntax.Entl.t =
  let up = entl.up in
  let (newpure_l, newspat_l) = entl.ant in
  let (newpure_r, newspat_r) = entl.suc in
  let ccant = CcSyntax.SH.create [] (List.map CcSyntax.var up) []
                (new2cc_pure newpure_l)
                (new2cc_spat newspat_l) in
  let ccsuc = [CcSyntax.SH.create [] [] []
                (new2cc_pure newpure_r)
                (new2cc_spat newspat_r)
              ] in
  { name = ""; ant = ccant; suc = ccsuc }
