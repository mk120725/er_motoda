(*--------------------------------*)
(* Tools				          *)
(*--------------------------------*)
exception Error_at of string;;
exception Skip;;
exception Break;;

let rec sum ls = if ls = [] then 0 else List.hd ls + sum (List.tl ls);;

let rec findOption cond ls = match ls with
  | [] -> None
  | hd::tail -> if cond hd then (Some hd) else findOption cond tail;;

let findPosOption x ls = 
  let rec findPosRec x' ls' n = match ls' with
  | [] -> None
  | hd::tail -> if x' = hd then Some n else findPosRec x' tail (n+1)
  in findPosRec x ls 0;;

let findItemOption key ls = 
   match findOption (fun (x,_) -> x = key) ls with
   | None -> None
   | Some (_,itm) -> Some itm;;

let dropRed ls = 
  let rec dropRedRec cur res = match cur with
    | [] -> List.rev res
    | hd::tail -> if (List.mem hd res) then dropRedRec tail res 
		else dropRedRec tail (hd::res)
  in dropRedRec ls [];;

let dropRedSorted equal ls =
  let rec dropRedSortedRec cur res = match cur with
    | [] -> List.rev res
    | x::cur' ->
       match res with
       | [] -> dropRedSortedRec cur' [x]
       | y::res' -> if equal x y then dropRedSortedRec cur' res
		    else dropRedSortedRec cur' (x::res)
  in dropRedSortedRec ls [];;
  
let rec unionLst ls1 ls2 = match ls2 with
  | [] -> ls1
  | x::ls2' -> if List.mem x ls1 then unionLst ls1 ls2'
	       else unionLst (x::ls1) ls2'
  
let isSubList ls1 ls2 = 
List.fold_right (fun x-> fun b -> (List.mem x ls2)&& b) ls1 true;;

let rec isSubListSorted order ls1 ls2 = match ls1,ls2 with
  | [], _ -> true
  | x::ls1', y::ls2' ->
     if order x y = 0 then isSubListSorted order ls1' ls2
     else if order x y > 0 then isSubListSorted order ls1 ls2'
     else false
  | _, _ -> false
					     
  
let countElemLst x lst = 
  let rec countElemLstRec y ls n = match ls with
    | [] -> n
    | hd::tail -> if y = hd then countElemLstRec y tail (n+1)
		  else countElemLstRec y tail n
  in countElemLstRec x lst 0;;

let isLinearList x ls = 
  let n = countElemLst x ls in
  if n = 1 then true else false;;
  
let rec elimElemLst x lst = match lst with
  | [] -> []
  | hd::tail -> if x = hd then elimElemLst x tail else hd::(elimElemLst x tail);;

let rec elimElemLstL xlst lst = match xlst with
  | [] -> lst
  | x::xlst' -> elimElemLstL xlst' (elimElemLst x lst);;

let interLst ls1 ls2 = 
  let rec interLstRec lst1 lst2 rest = match lst2 with
  | [] -> rest
  | x::xl -> if List.mem x lst1 then interLstRec lst1 xl (x::rest) 
	else interLstRec lst1 xl rest
  in List.rev(interLstRec ls1 ls2 []);;

(* zipLst [1;2;3] [a;b;c] is [(1,a);(2,b);(3,c)]*)
let rec zipLst ls1 ls2 = 
  match ls1,ls2 with
  | (hd1::tl1,hd2::tl2) -> (hd1,hd2)::(zipLst tl1 tl2)
  | (_,_) -> [];;

let rec zipLst3 ls1 ls2 ls3 =
  match ls1,ls2,ls3 with
  | (hd1::tl1,hd2::tl2,hd3::tl3) -> (hd1,hd2,hd3)::(zipLst3 tl1 tl2 tl3)
  | (_,_,_) -> [];;
  
(* genLst 2 3 is [4;3;2] *)
let rec genLstFrom i n =
  if n = 0 then [] else (n+i-1)::(genLstFrom i (n-1))

(* genLst 3 is [2;1;0] *)	
let genLst n = genLstFrom 0 n
	
let rec genNumLL n len = if len = 0 then [[]] else 
     let res = genNumLL n (len-1) in
     let lstN = List.rev (genLst n) in
     let addHd n = List.map (fun l->n::l) res in
     List.concat (List.map (fun x-> addHd x) lstN);;

let rec genNbools len = 
  let i2b n = if n = 0 then false else true in
  let iL2bL ls = List.map i2b ls in
  List.map iL2bL (genNumLL 2 len);;

let rec genNtrues len = 
  let i2b n = true in
  let iL2bL ls = List.map i2b ls in
  List.map iL2bL (genNumLL 1 len);;

let rec allTrueMes cond lst = 
	match lst with
	| [] -> None
	| hd::tail -> if cond hd then allTrueMes cond tail 
		        else Some hd;;

let allTrue cond lst : bool = match allTrueMes cond lst with
  | None -> true
  | Some _ -> false;;

(* makeCombPairs [1;2;3] returns [(1,2);(1,3);(2,3)]	*)
let rec makeCombPairs ls = match ls with
  | [] -> []
  | hd::tail -> 
     let ls1 = List.map (fun x-> (hd,x)) tail in
     let ls2 = makeCombPairs tail in
     List.append ls1 ls2;;

(* makeCombPairs2 [1;2] ['A';'B']returns [(1,'A');(1,'B');(2,'A');(2,'B')]*)
let rec makeCombPairs2 ls1 ls2 = 
  let makeCP1 ls x = List.map (fun v -> (v,x)) ls in
  List.concat (List.map (makeCP1 ls1) ls2)

let makeDiffPairs ls = 
  let rec makeDiffPairRec lst x = match lst with
    | [] -> []
    | y::yL -> let rest = makeDiffPairRec yL x in
	        if x = y then rest else (x,y)::rest
  in
  List.concat(List.map (makeDiffPairRec ls) ls)

(* Generating Fresh Variables *)
let genFreshVar sym vlst =   
  let rec genFreshVarRec s l n =
    let newVar = s^(string_of_int n) in
    if List.mem newVar l then genFreshVarRec s l (n+1) 
    else newVar
  in genFreshVarRec sym vlst 0

let rec genFreshVarL sym lst len = 
	if len = 0 then [] else
	let v = genFreshVar sym lst in
	v::(genFreshVarL sym (v::lst) (len-1))

let avoidCollisionOne vv w =
  if List.mem w vv then genFreshVar w vv else w

let avoidCollision vv ww = List.map (avoidCollisionOne vv) ww

let rec string_of_list (f: 'a -> string) sepsym strL = 
	List.fold_right 
	(fun x->fun y->
		if y = "" then f x
		else (f x)^sepsym^y)
	strL ""

let mapComma f strL = string_of_list f (",") strL

let concatStrLComma = mapComma (fun s -> s)

let mapNewLine f strL = string_of_list f ("\n") strL

let concatStrLNewLine = mapNewLine (fun s -> s)

let rec replLstNth n e ls = match ls with
  | [] -> []
  | hd::tl -> if n = 0 then e::tl else hd::(replLstNth (n-1) e tl)

let permute_optlist (ols : 'a option list) : 'a list option = 
	if List.exists (fun x-> x = None) ols then None else
	  let ll = List.map (fun x -> match x with None -> [] | Some y -> [y]) ols in
	Some (List.concat ll)

let func_option f (opt : 'a option) : 'b option = match opt with
  | None -> None
  | Some a -> Some (f a)

let getPos v lst = 
  let rec getPosRec x l n result = match l with
    | [] -> result
    | hd::tl -> 
	if x = hd then getPosRec x tl (n+1) (n::result) 
	else getPosRec x tl (n+1) result
  in List.rev (getPosRec v lst 0 [])

let rec isLinear lst = match lst with
  | [] -> true
  | x::xl -> if List.mem x xl then false else isLinear xl

let rec isLinearSorted ls =
  match ls with
  | [] -> true
  | x::ls' ->
	 match ls' with
	 | [] -> true
	 | y::ls'' -> if x = y then false else isLinearSorted ls'
		 
  
(* occurLst ["a";"b";"b";"b"] returns ["a0";"b0";"b1";"b2"]  *)
let occurLst1 ls = 
    let rec occurLstRec ls rest memo = 
      match rest with
      | [] -> ls
      | x::xl -> 
	 let n = countElemLst x memo in
	 occurLstRec (ls@[(x,n)]) xl (x::memo)
    in occurLstRec [] ls []

let occurLst ls = List.map (fun (x,n)->x^(string_of_int n)) (occurLst1 ls)

(* gatherKey2 [(1,"a","A");(2,"b","B");(1,"c","C");(1,"d","D")] returns [("a","A");("c","C");("d","D")]  *)
let gatherKey2 ls key = 
  let rec gatherKeyRec ls ky rest = match rest with
    | [] -> ls
    | (k,x,y)::xl -> if k = ky then gatherKeyRec (ls@[(x,y)]) ky xl else gatherKeyRec ls ky xl
  in gatherKeyRec [] key ls
  
let memEq eq x ls = List.exists (eq x) ls

let sublistEq eq ls1 ls2 = List.for_all (fun x -> memEq eq x ls2) ls1

let eqlistEq eq ls1 ls2 = (sublistEq eq ls1 ls2) && (sublistEq eq ls2 ls1)

let strhd str = String.get str 0

let strtl str = 
  let len = String.length str in
  String.sub str 1 (len-1)

let rec lexorder (ord : 'a -> 'a -> int) ls1 ls2 = 
  match (ls1, ls2) with
  | ([],[]) -> 0
  | (_,[]) -> 1
  | ([],_) -> -1
  | (_,_) -> 
       if ord (List.hd ls1) (List.hd ls2) > 0 then 1 
       else if ord (List.hd ls1) (List.hd ls2) < 0 then -1
       else lexorder ord (List.tl ls1) (List.tl ls2) 

let rec toCharList s = 
  let len = String.length s in
  if len = 0 then [] else 
  let hd = String.get s 0 in
  let tl = String.sub s 1 (len - 1) in
  hd::(toCharList tl)

	(*
let strlexorder s1 s2 = 
  let chorder c1 c2 = if c1 > c2 then 1 else if c1 < c2 then -1 else 0 in
  let s1' = toCharList s1 in
  let s2' = toCharList s2 in
  lexorder chorder s1' s2'
	*)
let strlexorder s1 s2 =
  if s1 = s2 then 0 else if s1 < s2 then -1 else 1

  
(* mergeL (>) [[1;4;7];[2;5;8];[3;6;9]] returns [1;2;3;4;5;6;7;8;9]*)
let mergeL order = List.fold_left (fun l -> fun l' -> List.merge order l l') []

let mkEqClass elems eql = 
  let n = List.length elems in
  let numL = List.rev (genLst n) in
  let table = ref (zipLst elems numL) in
  let gatherElemIndex n = 
    let lst = List.filter (fun (x,i) -> i = n) !table in
    List.map (fun (x,_) -> x) lst 
  in 
  let rec updateIndexRec i j tbl rest = match tbl with
    | [] -> List.rev rest
    | (e,k)::tbl' -> if i = k then updateIndexRec i j tbl' ((e,j)::rest)
		     else updateIndexRec i j tbl' ((e,k)::rest)
  in 
  let updateIndex i j = table := updateIndexRec i j !table [] in
  let updateTable (a,b) = 
    let idxa = List.assoc a !table in
    let idxb = List.assoc b !table in
    let min, max = min idxa idxb, max idxa idxb in
    updateIndex max min 
  in 
  List.iter updateTable eql;
  List.filter (fun ls -> ls <> []) (List.map gatherElemIndex numL)

let rec findClass clsL e = match clsL with
  | [] -> []
  | ls::clsL' -> if List.mem e ls then ls else findClass clsL' e

let mkClosure clsL eL =
  let rec mkClos1 rest elst = match elst with
    | [] -> rest
    | e::elst' ->
       mkClos1 (e::(findClass clsL e)@rest) elst'
  in dropRed (mkClos1 [] eL)

let disjoint ls1 ls2 =
  let answer = ref true in
  begin
	try
	  for i = 0 to List.length ls1 - 1 do
		if List.mem (List.nth ls1 i) ls2 then raise Break else ()
	  done;
	with
	  Break -> answer := false
  end;
  !answer

let setminus ls1 ls2 = List.filter (fun x -> not(List.mem x ls2)) ls1

let intersectL ll =
  let module L = List in
  if ll = [] then [] else
	let _res = ref (L.hd ll) in
	let _ll = ref (L.tl ll) in
	while !_res <> [] && !_ll <> [] do
	  _res := L.filter (fun x -> L.mem x (L.hd !_ll)) !_res;
	  _ll := L.tl !_ll;
	done;
	!_res
	  
let intersect l1 l2 = intersectL [l1;l2]
	
let subset ls1 ls2 = setminus ls1 ls2 = []

let seteq ls1 ls2 = (subset ls1 ls2) && (subset ls2 ls1)

let disjoint ls1 ls2 = intersect ls1 ls2 = []
  
let rec findDuplicate xl =
  match xl with
  | [] -> true
  | [x] -> true
  | x::(y::_ as yl) -> if x = y then false else findDuplicate yl

let splitString sym s =
  (* splitString "$$" "a$$b$$c" returns ["c";"b";"a"] *)
  let sym' = Bytes.of_string sym in
  let s' = Bytes.of_string s in
  let n = Bytes.length sym' in
  let max = Bytes.length s' in
  let _len = ref 0 in
  let _res = ref [] in
  let _start = ref 0 in
  let _i = ref 0 in
  begin
  try
	while true do
	  if !_i >= max then
		begin
		  _res := (Bytes.sub s' !_start !_len) :: !_res;
		  raise Break
		end
	  else if (!_i + n <= max && Bytes.sub s' !_i n = sym')
	  then
		begin
		  _res := (Bytes.sub s' !_start !_len) :: !_res;
		  _start := !_i + n;
		  _i := !_i + n;
		  _len := 0;
		end
	  else
		begin
		  _len := !_len + 1;
		  _i := !_i + 1;
		end
	done
  with
	Break -> ()
  end;
  List.map Bytes.to_string !_res

let mapNth f ls =
  (* mapNth (fun (x,i) -> x+i) [a;b;c] returns [f(a,0);f(b,1);f(c,2)] *)
  let indexL = List.rev (genLst (List.length ls)) in
  let lsidx = zipLst ls indexL in
  List.map f lsidx

let numberingLs ls = mapNth (fun x -> x) ls
	
let getInitSeg l k =
  (* getInitSeg [1;2;3;4] 3 returns ([1;2;3],[4]) *)
  let _res = ref l in
  let _seg = ref [] in
  for i = 0 to k - 1 do
	if !_res = [] then () else
	  begin
		_seg := (List.hd !_res)::!_seg;
		_res := List.tl !_res
	  end
  done;
  (List.rev !_seg,!_res)
;;

let isPrime n =
  let rec isNotDivisor d =
	d*d > n || (n mod d <> 0 && isNotDivisor (d+1))
  in
  isNotDivisor 2
;;

let nextPrime n =
  let rec aux i =
	if isPrime i then i else aux (i+1)
  in
  aux (n+1)
;;	

let genPrimes n =
  let _res = ref [] in
  let _cur = ref 1 in
  let _next = ref 1 in
  for i = 0 to n - 1 do
	_next := nextPrime !_cur;
	_res := !_next :: !_res;
	_cur := !_next
  done;
  !_res
;;

let enumSubsetBin n =
  let a = Array.make_matrix (n+1) (n+1) None in
  let rec ch m k =
    let _res = ref [] in
    match a.(m).(k) with
    | Some ll -> ll
    | None ->
       begin
         match m,k with
         | 0,0 -> _res := [[]];
         | _,_ when m < k -> _res := []
         | _,0 -> _res := List.map (fun xl -> 0::xl) (ch (m-1) 0)
         | _,_ ->
            let amk0 = List.map (fun xl -> 0::xl) (ch (m-1) k) in
            let amk1 = List.map (fun xl -> 1::xl) (ch (m-1) (k-1)) in
            _res := amk0 @ amk1
       end;
       a.(m).(k) <- Some !_res;
       !_res;
  in
  let _ans = ref [] in
  for i = 0 to n do
    let _ = ch n (n-i) in
    match a.(n).(n-i) with
    | None -> failwith ""
    | Some ll -> _ans := ll @ !_ans
  done;
  !_ans
;;

(* old one. just a backup *)
let enumSubsetIndex_old n =
  let bintoIndex bb =
	let ans = ref [] in
	let k = List.length bb in
	for i = 0 to k - 1 do
	  if List.nth bb (k-i-1) = 1 then ans := (k-i-1)::!ans else ()
	done;
	!ans
  in
  let kkk = enumSubsetBin n in
  List.map bintoIndex kkk
;;  

(* new one. enumulate all subsets of [0,..,n-1] *)
let enumSubsetIndex0 n =
  let ( << ) = (lsl) in
  let bitN = 1 << n in
  let _subset = ref [] in
  let _result = ref [] in
  for bitK = 0 to bitN - 1 do
    _subset := [];
    for i = 0 to n - 1 do
      match bitK land (1 << i) with
      | 0 -> () (* the i-th bit of bitK is 0 *)
      | _ -> _subset := i :: !_subset
    done;
    _result := !_subset :: !_result
  done;
  !_result
;;

let enumSubsetIndex n =
  let nnn = enumSubsetIndex0 n in
  let order nn1 nn2 =
    let len1 = List.length nn1 in
    let len2 = List.length nn2 in
    if len1 < len2 then -1 else if len1 > len2 then 1 else 0
  in
  List.sort order nnn
;;

let enumSubset ls =
  let module L = List in
  L.map (L.map (L.nth ls)) (enumSubsetIndex (L.length ls))
;;  

let num2bin n =
  (* num2bin 5 returns [1;0;1] (binary repl of 5) *)
  let _ls = ref [] in
  let _i = ref n in
  while !_i <> 0 do
	_ls := (!_i mod 2) :: !_ls;
	_i := !_i / 2;
  done;
  !_ls
;;

let nthSubset ls n =
  (* nthSubset [1;2;3] 3 returns [2;3] (3rd subset of [1;2;3]) *)
  let rec aux res bls sl =
	match bls,sl with
	| [],_ -> res
	| _,[] -> failwith "nthSubset: out of length"
	| 1::bls',a::sl' -> aux (a::res) bls' sl'
	| _::bls',_::sl' -> aux res bls' sl'
  in
  aux [] (List.rev (num2bin n)) (List.rev ls)
;;

let splitListByLength numL ls =
  let rec aux n numL1 res resL ll =
	match n,numL1 with
	| 0,[] -> List.rev ((List.rev res)::resL)
	| 0,n1::numL2 -> aux n1 numL2 [] ((List.rev res)::resL) ll
	| _,_ -> aux (n-1) numL1 ((List.hd ll)::res) resL (List.tl ll)
  in
  aux (List.hd numL) (List.tl numL) [] [] ls
;;

module List_tailrec = struct

  let rec rev1 res ls = match ls with
    | [] -> res
    | x::ls' -> rev1 (x::res) ls'

  let rev ls = rev1 [] ls 

  let rec append_rev ls1 ls2 = 
   match ls1 with 
   | [] -> ls2
   | x::ls1 -> append_rev ls1 (x::ls2)

  let append ls1 ls2 = 
    let ls1rev = rev ls1 in
    append_rev ls1rev ls2

  let rec map_rev1 f ls result = match ls with
    | [] -> result
    | x::xl -> map_rev1 f xl ((f x)::result)

  let map_rev f ls = map_rev1 f ls []

  let map f ls = rev (map_rev f ls)

  let rec concat_rev1 res ll = match ll with
    | [] -> res
    | hdL::ll' ->
       let res' = append_rev hdL res in
       concat_rev1 res' ll'

  let concat_rev ll = concat_rev1 [] ll

  let concat ll = rev (concat_rev ll)
		      
(* allChoice [[1;2];[3;4];[5;6]] returns *)
(* [1;3;5];[1;3;6];[1;4;5];[1;4;6]	*)
(* [2;3;5];[2;3;6];[2;4;5];[2;4;6]	*)
  let rec allChoice1 resll ll = match ll with
  | [] -> map rev resll
  | hdL::ll' ->
     let resll' = concat (map (fun i -> map (fun rl -> i::rl) resll) hdL) in
     allChoice1 resll' ll'

  let allChoice ll = allChoice1 [[]] ll


let rec allChoiceBool1 res ll =
  match ll with
  | [] -> map (fun (b,l) -> (b,rev l)) res
  | (ls1,ls2)::ll' ->
     let res1 = concat (map_rev (fun x-> (map_rev (fun (b,l) -> (b,x::l)) res)) ls1) in
     let res2 = concat (map (fun x-> (map (fun (b,l) -> (true,x::l)) res)) ls2) in
     let res' = append_rev res1 res2 in
     allChoiceBool1 res' ll'

let allChoiceBool ll = allChoiceBool1 [(false,[])] ll

(* allChoiceTwo [([1;2],[3;4]);([5;6],[7;8])] returns lists	*)
(* similar to the result of allChoice [[1;2;3;4];[5;6;7;8]],	*)
(* except for the choices from [1;2] and [5;6]			*)
(* That is, it returns						*)
(* [1;7];[1;8];[2;7];[2;8];					*)
(* [3;5];[3;6];[3;7];[3;8];[4;5];[4;6];[4;7];[4;8];		*)
let allChoiceTwo ll =
  let llb = allChoiceBool ll in
  List.fold_left (fun xll -> fun (b,l) -> if b then l::xll else xll) [] llb;;

(* dropsame order ls1 ls2 returns a sublist of ls1, which is obtained by dropping the elements in ls2 *)
(* ls1 and ls2 are assumed to be sorted w.r.t. order *)
let rec dropsame1 order res ls1 ls2 = match ls1,ls2 with
	| [],_ -> rev res
	| a::ls1',b::ls2' ->
	   if order a b < 0 then dropsame1 order (a::res) ls1' ls2
	   else if order a b = 0 then dropsame1 order res ls1' ls2
	   else dropsame1 order res ls1 ls2'
	| _,[] -> append_rev res ls1

let dropsame order ls1 ls2 = dropsame1 order [] ls1 ls2

let takeNth ls i = 
  let rec takeNthRec res lst pos =
    if lst = [] then failwith "takeNth" else 
    let hd = List.hd lst in
    let tl = List.tl lst in
    match pos with
    | 0 -> (hd, List.rev_append res tl)
    | _ -> takeNthRec (hd::res) tl (pos-1)
  in takeNthRec [] ls i

let dropNth ls i = let (_,res) = takeNth ls i in res
				       
let rec allChoiceApply1 f chkfunc ll res = match ll with
  | [] -> f(List.rev res)
  | ls::ll' ->
     for i = 1 to List.length ls do
       try
	 allChoiceApply1 f chkfunc ll' (chkfunc ((List.nth ls (i-1))::res))
       with Skip -> ()
     done

let allChoiceApply f chkfunc ll = allChoiceApply1 f chkfunc ll []

let permApply f ls =
  let rec permApplyRec res len lst =
    if len = 0 then f res
    else
      for i = 0 to len - 1 do
	  let (x,lst') = takeNth lst i in
	  permApplyRec (x::res) (len-1) lst'
      done
  in
  permApplyRec [] (List.length ls) ls

let permApply2 f ls1 ls2 =
  permApply (fun ls -> permApply (f ls) ls2) ls1

(* permApplyL f lls makes all possible permutations of	*)
(* the lists of lls then apply f to it			*)
(* e.g. permApplyL f [[1;2];[3;4]] performs		*)
(* f [[1;2];[3;4]]					*)
(* f [[1;2];[4;3]]					*)
(* f [[2;1];[3;4]]					*)
(* f [[2;1];[4;3]]					*)
let permApplyL f ll =
  let rec permApplyR rest restl ls lls =
    match ls,lls with
    | [],[] -> f (List.rev ((List.rev rest)::restl))
    | [],ls1::lls1 -> permApplyR [] ((List.rev rest)::restl) ls1 lls1
    | _,_ ->
      for i = 0 to List.length ls - 1 do
	let (a,ls') = takeNth ls i in
	permApplyR (a::rest) restl ls' lls
      done
  in
  match ll with
  | [] -> f []
  | ls1::ll1 -> permApplyR [] [] ls1 ll1

let filter cond l =
  let rec filterR rest0 rest1 ls =
    match ls with
    | [] -> (List.rev rest0,List.rev rest1)
    | x::ls' ->
      if cond x then
	filterR rest0 (x::rest1) ls'
      else
	filterR (x::rest0) rest1 ls'
  in
  filterR [] [] l

let applyConcatNth f ls n =
  let rec apply res l i =
    match l with
    | [] -> (List.rev res) @ l
    | x::xl ->
      if i = 0 then
	(List.rev res) @ f x @ xl
      else
	apply (x::res) xl (i - 1)
  in
  apply [] ls n

let applyNth f = applyConcatNth (fun x -> [f x]) 

(* replaceNth [10;20;30] 1 [40;50] returns [10;40;50;30] *)
let replaceNth ls n xl =
  applyConcatNth (fun _ -> xl) ls n

let mkAllMapTwoGroup func ls1 ls2 =
  let rec aux res xl yl =
    match xl,yl with
    | _,[] -> res
    | [],_::yl1 -> aux res ls1 yl1
    | x::xl1,y::_ -> aux ((func (x,y))::res) xl1 yl
  in
  aux [] ls1 ls2

let mkAllMapGroup func ll =
  let res = ref [] in
  for i = 0 to List.length ll - 1 do
    let xl = List.nth ll i in
    for j = i+1 to List.length ll - 1 do
      let yl = List.nth ll j in
      res := (mkAllMapTwoGroup func xl yl) @ !res
    done;
  done;
  !res
  
end
;;
