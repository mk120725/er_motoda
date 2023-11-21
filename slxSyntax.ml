(* Syntax for Extended Symbolic Heap Language     *)
(* A SH of this language has the following form   *)
(* Up(x,y) && x=y & x <> Nil && x -> (z) * P(z,w) *)
open Tools

module SHterm = SlSyntax.SHterm
module SHpureExp = SlSyntax.SHpureExp
module SHpure = SlSyntax.SHpure
module SHspatExp = SlSyntax.SHspatExp
module SHspat = SlSyntax.SHspat
  
module SH = struct

(* (up,pure,spat) means up && pure && spat *)
  type t = string list * SHpure.t * SHspat.t 

  let to_string (it : t) = 
    let (vv,pp,ss) = it in
	let upStr = string_of_list (fun x->x) "," vv in
    let shStr = SlSyntax.SH.to_string (pp,ss) in
    match vv with
    | [] -> shStr
    | _ -> upStr^" & "^shStr

  let print (it : t) = print_string (to_string it)

  let forget (up,pp,ss) : SlSyntax.SH.t = (pp,ss)
	
end
;;

module IS = struct
	
  type t = (string * string list * SH.t list) list

  let forget is =
	let forget_ID (pr,prm,hh) = (pr,prm,List.map SH.forget hh) in
	List.map forget_ID is
	
end
;;

