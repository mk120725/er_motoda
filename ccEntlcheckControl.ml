(*-------------------------------------------*)
(* Control                                   *)
(*-------------------------------------------*)
(*
- Main (top level)
- MainLoop (called by Main)
-- controlled by ControlStack (Cstack)
--- Cstack
--- Subgoal(J): next subgoal (sg)
--- Split(J,a,b): split-branch
---- sgs after a split-branch are those of the branch, 
---- that is, they are disposed when the branch is invalid
*)
open Tools
open CcSyntax
open CcWand
open CcEntlcheck
   
module Opt = Options.Cycomp

exception MainLoopFail
exception BranchSuccess
exception BranchFailure
exception Invalid
  
module SplitBranch = struct
	
  type pos =
	{
	  mutable left : bool;
	  mutable idx : int list;
	  mutable cnt : int
	(* Current position of a split branch                               *)
	(* Example: {left=true;idx=[1;3];cnt=3}                             *)
	(* Meaning: F1 |- A1,C1 of F1*F2 |- A1*A2,B1*B2,C1*C2 is processing *)
	(* 'left' : the left-side (F1,A1,B1,C1) is currently handled        *)
	(* 'idx' : {A1,C1} is represented by [1;3]                          *)
	(* 'cnt' : counter of finished branches (<= 2^|{1,2,3}|)            *)
	}
  let to_string_pos pos =
	let lrS = if pos.left then "L" else "R" in
	let idxS = "[" ^ (string_of_list string_of_int "," pos.idx) ^ "]" in
	let cntS = string_of_int pos.cnt in
	"{" ^ lrS ^ "; index:" ^ idxS ^ "; count:" ^ cntS ^ "}"
	  
  type sign = V | I | U | D
  (* V : Valid *)
  (* I : Invalid *)
  (* U : Unsolved *)
  (* D : Don't care *)

  let to_string_sign =
	function
	| V -> "V"
	| I -> "I"
	| U -> "U"
	| D -> "D"
	
  type state = (int list * sign) array
  (* Current state of a split branch                                *)
  (* Example: [| ([],I); ([1],V); ([2],D); ([1;2],U) |]             *)
  (* Meaning: Assume it is a state for left of F1*F2 |- A1*A2,B1*B2 *)
  (* - F1 |-       is Invalid    *)
  (* - F1 |- A1    is Valid      *)
  (* - F1 |- B1    is Don't care *)
  (* - F1 |- A1,B1 is Unsolved   *)

  let to_string_state a =
	let _res = ref "" in
	for i = 0 to Array.length a - 1 do
	  _res := !_res ^ (to_string_sign (snd a.(i)))
	done;
	!_res

  let to_string_state_rev a =
	let _res = ref "" in
	for i = 0 to Array.length a - 1 do
	  let j = Array.length a - i - 1 in
	  _res := !_res ^ (to_string_sign (snd a.(j)))
	done;
	!_res
	  
  let println_state a = print_endline (to_string_state a)
	
  let nthIdx (a : state) i = fst (a.(i))

  let nthSign (a : state) i = snd (a.(i))

  let sizePowIdx (a : state) = Array.length a

  let getMaxIdx a = fst(a.(sizePowIdx a - 1))
	
  let sizeMaxIdx a = List.length (getMaxIdx a)
	
  let setByIdx a idx sign =
	for i = 0 to sizePowIdx a - 1 do
	  if nthIdx a i = idx then a.(i) <- (idx,sign) else ()
	done
	
  let getByIdx_opt a idx =
	let res = ref None in
	begin
	  try
		for i = 0 to sizePowIdx a - 1 do
		  if nthIdx a i = idx then (res := Some (nthSign a i); raise Exit) else ()
		done;
	  with Exit -> ()
	end;
	!res

  let getByIdx a idx =
	(* It returns S such that a = [| ... (idx,S) ... |] *)
	match getByIdx_opt a idx with
	| Some x -> x
	| None -> failwith "getByIdx: NotFound"

(*
  let updatePos a b pos =
	(* It updates pos, that is,                                   *)
	(* it finds the left most index of a and b which contains 'U' *)
	(* Example:                                                   *)
	(* a = [| ([],V); ([0],I); ([1],U); ([0;1],U)|]               *)
	(* b = [| ([],I); ([0],U); ([1],V); ([0;1],U)|]               *)
	(* Then pos becomes {left = false, idx = [0], cnt = pos.cnt}  *)
	let curIdx = ref [] in
    let j = ref 0 in
	try
	  for s = 0 to sizeMaxIdx a do
		curIdx := nthIdx a !j;
		match L.length !curIdx <= s, nthSign a !j, nthSign b !j with
		| true,U,_ -> pos.left <- true; pos.idx <- !curIdx; raise Exit
		| true,_,U -> pos.left <- false; pos.idx <- !curIdx; raise Exit
		| _,_,_ -> j := !j + 1
	  done;
	  failwith "updateNextPos: NoNextPosition"
	with
	  Exit -> ()
*)
  let updatePos a b pos =
	(* It updates pos, that is,                                   *)
	(* it finds the left most index of a and b which contains 'U' *)
	(* Example:                                                   *)
	(* a = [| ([],V); ([0],I); ([1],U); ([0;1],U)|]               *)
	(* b = [| ([],I); ([0],U); ([1],V); ([0;1],U)|]               *)
	(* Then pos becomes {left = false, idx = [0], cnt = pos.cnt}  *)
	let curIdx = ref [] in
	try
	  for s = 0 to sizeMaxIdx a do
        for j = 0 to Array.length a - 1 do
		  curIdx := nthIdx a j;
		  match L.length !curIdx = s, nthSign a j, nthSign b j with
		  | true,U,_ -> pos.left <- true; pos.idx <- !curIdx; raise Exit
		  | true,_,U -> pos.left <- false; pos.idx <- !curIdx; raise Exit
		  | _,_,_ -> ()
	    done;
      done;
	  failwith "updateNextPos: NoNextPosition"
	with
	  Exit -> ()
            
  let updateSuccAB a b pos =
	(* It updates states 'a' and 'b' with 'pos'                      *)
	(* It's called when a current split-branch finishes with SUCCESS *)
	(* Example:                                                      *)
	(* a = [|    ([],I); ([0],U); ([1],U); ([0;1],U) |]              *)
	(* b = [| ([0;1],U); ([1],I); ([0],U);    ([],U) |]              *)
	(* pos = {left = true; idx = [0]; cnt = 1}                       *)
	(* This function is called when F1 |- A1 is proved VALID         *)
	(* Then it updates a,b,pos as follows                            *)
	(* a = [|    ([],I); ([0],'V'); ([1],U); ([0;1],'V') |]          *)
	(* b = [| ([0;1],U); ([1],'D'); ([0],U);    ([],'D') |]          *)
	(* pos = {left = 'false'; idx = [0]; cnt = '2'}                  *)
	(* - For each supset I' of [0], change a[I'] to V, b[I-I'] to D  *)
	(* - Then cnt is incremented. Finish with succeed if cnt = 4     *)
	let maxIdx = getMaxIdx a in
	let (c,c') = if pos.left then (a,b) else (b,a) in
	for i = 0 to Array.length a - 1 do
	  let curIdx = nthIdx c i in
	  match subset pos.idx curIdx with
	  | false -> ()
	  | true ->
		 begin
		   c.(i) <- (curIdx,V);
		   let curIdx' = setminus maxIdx curIdx in
		   match getByIdx c' curIdx' with
		   | U | I ->
			  begin
				setByIdx c' curIdx' D;
				pos.cnt <- pos.cnt + 1;
			  end
		   | _ -> setByIdx c' curIdx' D
		 end
	done
  ;;
	  
  let updateFailAB a b pos =
	(* It updates states 'a' and 'b' with 'pos'                      *)
	(* It's called when a current split-branch finishes with FAILURE *)
	(* Example:                                                      *)
	(* a = [|    ([],I); ([0],U); ([1],U); ([0;1],U) |]              *)
	(* b = [| ([0;1],U); ([1],I); ([0],U);    ([],U) |]              *)
	(* pos = {left = true; idx = [0]; cnt = 1}                       *)
	(* This function is called when F1 |- A1 is proved INVALID       *)
	(* Then it updates a,b,pos as follows                            *)
	(* a = [|    ([],I); ([0],'I'); ([1],U); ([0;1],U) |]            *)
	(* b = [| ([0;1],U); ([1], I ); ([0],U);    ([],U) |]            *)
	(* This case both a[{0}] and b[{1}] are Invalid.                 *)
	(* So the branch finishes with Failure                           *)
	let maxIdx = getMaxIdx a in
	let (c,c') = if pos.left then (a,b) else (b,a) in
	let curIdx = pos.idx in
	let curIdx' = setminus maxIdx curIdx in
	setByIdx c curIdx I;
	match getByIdx c' curIdx' with
	| I -> raise BranchFailure
	| _ -> ()
  ;;

  let mkInitPos () = { left = true; idx = []; cnt = 0 }

  let mkInitState (ge : GrpEntl.t) : state =
	(*
	let idxL = enumSubsetIndex (GrpEntl.numGroup ge) in
	 *)
	let idxL = enumSubsetIndex (L.length ge.GrpEntl.suc) in
	Array.init (L.length idxL) (fun j -> (L.nth idxL j, U))

  let mkInitSub (ge : GrpEntl.t) =
    let pow n =
      let _res = ref 1 in
      for i = 0 to n - 1 do
        _res := 2 * !_res
      done;
      !_res
    in
    let size = List.length (ge.GrpEntl.suc) in
    (pow size) - 1
    
end
;;

module SB = SplitBranch

module History = struct
  type t = Entl.t list

  let to_string (hsty : t) =
    let mes1 = "% CURRENT HISTORY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" in
    let mes2 = "% CURRENT HISTORY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" in
    let body = string_of_list Entl.to_string "\n" hsty in
    mes1 ^ "\n" ^ body ^ "\n" ^ mes2 ^ "\n"     

end

module Ctrl = struct
  (* Control type *)
  (* 1. subgoal (J,idx)       *)
  (* 2. split-branch          *)
  (* idx is the path of J in the proof *)
  type t =
	(* Subgoal *)
	| Subgl of Entl.t * int list
	(* Split-branch(virgin-flag,entl,sub,stateA,stateB,position,history) *)
	(* virgin-flag = true means that the split is a first-looked one *)
    (* sub indicates current subset-entailment of entl *)
	| Split of bool * GrpEntl.t * int * SB.state * SB.state * SB.pos * History.t

  let to_string c =
    match c with
    | Subgl (e,idx) ->
       "Subgl: \n"
       ^ (Entl.to_string e)
    | Split (true,e,sub,a,b,pos,hsty) ->
       "SplitT: \n"
       ^ (GrpEntl.to_string e) ^ "\n"
       ^ ":subsetID=" ^ (string_of_int sub) ^ "\n"
       ^ ":" ^ (SB.to_string_state a) ^ " (LtoR)\n"
       ^ ":" ^ (SB.to_string_state_rev b) ^ " (RtoL)"
    | Split (false,e,sub,a,b,pos,hsty) ->
       "SplitF: \n"
       ^ (GrpEntl.to_string e) ^ "\n"
       ^ ":subsetID=" ^ (string_of_int sub) ^ "\n"
       ^ ":" ^ (SB.to_string_state a) ^ "(LtoR)\n"
       ^ ":" ^ (SB.to_string_state_rev b) ^ "(RtoL)"

  let println c = print_endline @@ (to_string c)

end	  


module CtrlStack = struct
	
  type t = Ctrl.t list

  let rec revert (cStack : t) : t =
	(* revert [s1;s2;s3;s4] returns [s3;s4] if s1,s2 are subgoals and s3 is split *)
	match cStack with
	| [] ->
	   begin
		 Opt.sayifDebug @@ "MainLoop fails: No next subgoal\n";
		 raise MainLoopFail
	   end
	| Ctrl.Subgl _ :: cStack1 -> revert cStack1
	| Ctrl.Split(true,_,_,_,_,_,_) :: cStack1 -> revert cStack1
	| Ctrl.Split(false,_,_,_,_,_,_) :: _ -> cStack

  let to_string (cStack : t) =
    let mes1 = "# CURRENT CONTROL STACK ############################################" in
    let mes2 = "# CURRENT CONTROL STACK ############################################" in
    let body = string_of_list Ctrl.to_string "\n" cStack in
    mes1 ^ "\n" ^ body ^ "\n" ^ mes2 ^ "\n" 
    
  let println (cStack : t) = print_endline (to_string cStack)
                                        
end

module CS = CtrlStack

let ccEmpcheck (e : Entl.t) =
  (* it returns "Some true" if e is validiated by Emp-rule  *)
  (* returns "Some false" if LHS of e is Emp but RHS is not *)
  (* returns "None" otherwise                               *)
  (* e is assumed that its Pures in RHS are non-Eq          *)
  let isEmpSH h = (L.length h.SH.gm = 1 && fst (L.hd h.SH.gm) = S.Emp) || L.length h.SH.gm = 0 in
  match isEmpSH e.Entl.ant, L.exists isEmpSH e.Entl.suc with
  | true,true -> Some true
  | true,false -> Some false
  | _,_ -> None

let mkSplitAssump (ge : GrpEntl.t) pos =
  let (id,h,kk) = GE.decomp ge in
  let kkNum = numberingLs kk in
  let kkNum1 = L.filter (fun (k,i) -> L.mem i pos.SB.idx) kkNum in
  let kk1 = L.map fst kkNum1 in
  let (xx,yy,oo,pp,gg) = SH.decomp h in
  let (gg1,gg2) = if pos.SB.left then ([L.hd gg],L.tl gg) else (L.tl gg,[L.hd gg]) in
  let ttOtherCell = GG.getRootCells gg2 in
  let tRootCells = GG.getRootCells gg1 in
  let yy1 = dropRed (ttOtherCell @ yy) in
  let h2 = SH.create xx yy1 oo pp gg1 in
  let kk2 = L.map (GrpSHs.filterRoots tRootCells) kk1 in
  GE.create id h2 kk2
;;	

(* making split assumption for subset-split *)
let mkSplitAssumpSub (ge : GrpEntl.t) sub pos =
  let ge1 = GrpEntl.mkSubEntl ge sub in
  mkSplitAssump ge1 pos
;;

let _H = ref [] (* history *)

let _ctrlStack = ref [] (* control stack *)

let _entl = ref (Entl.mkDummy ())

let _yyOrig = ref [1]
          
(* MainLoop *)
let ccMainLoop (dd : IndSys.t) dwand (e,idx) =
  Opt.sayifDebug "\n==> MainLoop start";
  (* It may raise MainLoopFail *)
  _entl := e;
  _ctrlStack := [ Ctrl.Subgl (e,idx) ];
  let _branchFlag = ref true in
  let ddOrig = dd in
  let (ddWand,contraIP) = updateIndSys2 dwand dd in
  Opt.sayifDebug @@ "[Inductive-Wand Definitions]";
  Opt.sayifDebug @@ (IndSys.to_string ddWand);
  Opt.sayifDebug @@ "Contradict Predicates: " ^ (string_of_list (fun x -> x) "," contraIP) ^ "\n";

  (* IndDefAnalysis を入れてみた部分 要検証 *)
  let _yyWand = ref [] in
  let _uu = ref [] in
  Opt.doifOpt @@ (fun _ -> 
    begin
      _yyWand := CcIndDefAnalysis.analyzeIS ddWand;
      let vvWand = CcFactorAnalysis.computeYY ddWand in
      _uu := CcFactorAnalysis.fromYYtoUU vvWand 
    end);
  (* branch flag *)
  (* None       : the current branch is not handled yet           *)
  (* Some true  : the branch is in handling and doesn't fail yet  *)
  (* Some false : the branch is in handling and fail              *)
  while !_ctrlStack <> [] do
	try (* try for Skip and BranchFailure *)
      Opt.sayifDebug @@ CtrlStack.to_string !_ctrlStack;
      Opt.sayifDebug @@ History.to_string !_H;
	  let ctrl = L.hd !_ctrlStack in
	  _ctrlStack := L.tl !_ctrlStack;
	  match ctrl with
	  (* Case Split1: Not-virgin case. The result of the split-branch is already obtained *)
	  | Ctrl.Split(false,ge1,sub,a1,b1,pos1,hstry1) ->
		 begin
		   try
			 let resultMes = if !_branchFlag then "Valid" else "Invalid" in
			 Opt.sayifDebug @@ "===> Returned from a SplitBranch of " ^ (GrpEntl.to_string_id ge1) ^ " with " ^ resultMes;
             begin
               match !_branchFlag with
               | true -> (* Valid case for a1,b1,pos1 *)
                  SB.updateSuccAB a1 b1 pos1
               | false -> (* Invalid case for a1,b1,pos1 *)
                  try SB.updateFailAB a1 b1 pos1 with
                  | BranchFailure ->
                     begin
                       Opt.doifNotFull @@ (fun _ -> raise BranchFailure);
                       if sub <= 1 then raise BranchFailure
                       else
                         let ge1sub = GrpEntl.mkSubEntl ge1 (sub-1) in
                         let a1sub = SB.mkInitState ge1sub in
                         let b1sub = SB.mkInitState ge1sub in
                         let pos1sub = SB.mkInitPos () in
                         _ctrlStack := Ctrl.Split(true,ge1,sub-1,a1sub,b1sub,pos1sub,hstry1) :: !_ctrlStack;
                         raise Skip;
                     end;
             end;

			 Opt.sayifDebug @@ "Current state:";
			 Opt.sayifDebug @@ "a[]: " ^ (SB.to_string_state a1) ^ " (LtoR)";
			 Opt.sayifDebug @@ "b[]: " ^ (SB.to_string_state_rev b1) ^ " (RtoL)";
			 Opt.sayifDebug @@ "pos: " ^ (SB.to_string_pos pos1);
             Opt.sayifDebug @@ "sub: " ^ (string_of_int sub) ^ "\n";
			 
			 if !_branchFlag && pos1.SB.cnt = Array.length a1 then raise BranchSuccess else ();
        	 SB.updatePos a1 b1 pos1;
			 
			 _ctrlStack := Ctrl.Split(true,ge1,sub,a1,b1,pos1,hstry1) :: !_ctrlStack;
			 _H := hstry1;
			 _branchFlag := true
		   with BranchSuccess ->
			 begin
			   Opt.sayifDebug @@ "Branch Success " ^ (GrpEntl.to_string_id ge1) ^ "\n";
			   ()
			 end
		 end
		   
	  (* Case Split: Virgin case. The split-branch is handled from now *)
	  | Ctrl.Split(true,ge1,sub,a1,b1,pos1,hstry1) ->
		 begin
   		   Opt.sayifDebug @@ "===> Start Operating SplitBranch of " ^ (GrpEntl.to_string_id ge1);
		   let ge2 = mkSplitAssumpSub ge1 sub pos1 in
		   ge2.GE.name <- ge2.GE.name ^ "S";
		   Opt.sayifDebug @@ "Split Assumption:";
		   Opt.sayifDebug @@ GrpEntl.to_string ge2;
		   Opt.sayifDebug0 @@ "Checking Single Group ---> ";
		   match GE.isSingleGroup ge2 with
		   | true ->
			  begin
   				Opt.sayifDebug @@ "true (It becomes the next subgoal)\n";
				let e1 = ccNormalize (GE.fromSingleGroup ge2) in
				Opt.sayifDebug @@ (Entl.to_string_id e1) ^ " is pushed to the ControlStack\n";
				_ctrlStack := Ctrl.Subgl(e1,[]) :: Ctrl.Split(false,ge1,sub,a1,b1,pos1,hstry1) :: !_ctrlStack;
			  end
		   | false ->
			  begin
				Opt.sayifDebug @@ "false";
				let pos2 = SB.mkInitPos () in
				let a2 = SB.mkInitState ge2 in
				let b2 = SB.mkInitState ge2 in
                let sub2 = SB.mkInitSub ge2 in
				_ctrlStack := Ctrl.Split(true,ge2,sub2,a2,b2,pos2,hstry1) :: Ctrl.Split(false,ge1,sub,a1,b1,pos1,hstry1) :: !_ctrlStack;
			  end
		 end
		   
	  (* Case Subgl *)
	  | Ctrl.Subgl(e1,idx) ->
		 begin
		   _entl := e1;
		   Opt.sayifDebug @@ "Current Subgoal:";
		   Opt.sayifDebug @@ Entl.to_string !_entl;
		   
		   Opt.sayifDebug0 @@ "Identity checking ---> ";
		   if trivCheck !_entl
		   then
			 begin
			   Opt.sayifDebug "true (This branch is successfully finished)\n";
			   raise Skip
			 end
		   else
			 begin
			   Opt.sayifDebug "false";
			   ()
			 end;
		   
		   Opt.sayifDebug0 @@ "Bud-Companion check " ^ (Entl.to_string_id !_entl) ^ "--> ";
		   if ccBudSimple e1 !_H
		   then
			 begin
			   Opt.sayifDebug "true (This branch is successfully finished)\n";
			   raise Skip
			 end
		   else
			 begin
			   Opt.sayifDebug "false";
			   ()
			 end;

		   Opt.sayifDebug0 @@ "Checking Right Emptiness " ^ (Entl.to_string_id !_entl) ^ "--> ";
		   if !_entl.Entl.suc = []
		   then
			 begin
			   Opt.sayifDebug @@ "empty (This branch fails)";
			   raise BranchFailure
			 end
		   else
			 begin
			   Opt.sayifDebug @@ "not empty\n";
			   ()
			 end;

		   _H := e1 :: !_H;
		   let (id,h,hh) = Entl.decomp !_entl in
		   if intersectL (L.map SH.getRoots (h::hh)) = []
		   then
			 let tRoot = L.hd (SH.getRoots h) in
			 for i = 0 to L.length hh - 1 do
			   try
				 let hi = L.nth hh i in
				 if L.mem tRoot (SH.getRoots hi) then raise Skip else
				   for j = 0 to L.length hi.SH.gm - 1 do
					 Opt.sayifDebug0 @@ "Finding Common Root --> ";
					 if L.mem tRoot (snd (L.nth hi.SH.gm j)) then
					   begin
						 Opt.sayifDebug "No common root (do factor)";
						 Opt.sayifDebug @@ "\n===> Factor for " ^ (Entl.to_string_id !_entl) ^ "with " ^ (T.to_string tRoot);
						 _entl := ccFactor ddOrig ddWand dwand (i,j) tRoot !_entl;
						 !_entl.Entl.name <- (!_entl.Entl.name ^ "f");
						 Opt.sayifDebug "Factor result:";
						 Opt.sayifDebug @@ Entl.to_string !_entl ^ "\n";
						 raise Skip
					   end
					 else
					   Opt.sayifDebug "Common root found (skip Factor)\n";
				   done;
			   with Skip -> ()
			 done;
		   else ();
           (*----*)
		   Opt.sayifDebug @@ ">>> Simplification of the factor result";
           _entl := instEx !_entl;
           !_entl.Entl.suc <- L.filter (fun h -> PP.satcheck (!_entl.E.ant.SH.pi @ h.SH.pi)) !_entl.Entl.suc;
           !_entl.Entl.suc <- L.map dropTrivEq !_entl.Entl.suc;
		   Opt.sayifDebug @@ Entl.to_string !_entl ^ "\n";
           (*----*)
           Opt.sayifDebug ">>> Reducing Contradict Inductive Predicates";
           _entl := reduceContradictPreds contraIP !_entl;
		   Opt.sayifDebug @@ Entl.to_string !_entl ^ "\n";
		   (*----*)
           Opt.doifOpt @@ (fun _ ->
             begin
               Opt.sayifDebug @@ ">>> Reducing by Right Contradiction checking";
               _entl := reducingRight !_yyWand !_entl;
               Opt.sayifDebug @@ Entl.to_string !_entl ^ "\n";
               (*----*)
               Opt.sayifDebug ">>> Reducing by IndDef Analysis";
		       _entl := CcFactorAnalysis.updateEntlWithUU !_uu !_entl;
		       Opt.sayifDebug @@ Entl.to_string !_entl ^ "\n";
             end);
           (*----*)
           
		   Opt.sayifDebug @@ "===> Unfold for " ^ (Entl.to_string_id !_entl);
		   let eeUnfold = ccUnfold ddWand !_entl in
		   Opt.sayifDebug "Unfold result(s):";
		   Opt.doifDebug (fun _ -> L.iter Entl.println eeUnfold) ();

		   Opt.sayifDebug ">>> Reducing by checking Groups";
		   let eeUnfold0 = L.map reduceByCheckingGroup eeUnfold in
		   Opt.doifDebug (fun _ -> L.iter Entl.println eeUnfold0) ();

           Opt.sayifDebug @@ ">>> Reducing by Left Unsat checking";
           let eeUnfold1a = L.filter (fun e -> CcSatcheck.decideSatSH ddWand e.Entl.ant) eeUnfold0 in
		   Opt.doifDebug (fun _ -> L.iter Entl.println eeUnfold1a) ();
		   Opt.sayifDebug "";

           Opt.sayifDebug @@ ">>> Reducing by Right Contradiction checking";
           let _eeTmp = ref eeUnfold1a in
           Opt.doifOpt @@ (fun _ -> 
             begin
               _eeTmp := L.map (reducingRight !_yyWand) !_eeTmp
             end);
		   Opt.doifDebug (fun _ -> L.iter Entl.println !_eeTmp) ();
		   Opt.sayifDebug "";
           
		   for j = 0 to L.length !_eeTmp - 1 do       
		     let ej = L.nth !_eeTmp j in
			 let ttFV = L.map var (Entl.fv ej) in
			 let ttRootsj = SH.getRoots ej.Entl.ant in
			 let ttOthersj = setminus ttFV ttRootsj in
			 let pppKj1 = mkEqNeqAll2 ttRootsj ttOthersj in
			 let pppKj2 = L.map (fun pp -> PP.nf (ej.Entl.ant.SH.pi @ pp)) pppKj1 in
			 let pppKj3 = L.filter PP.simpleCheckCons pppKj2 in
			 let pppKj4 = L.map PP.nf pppKj3 in
			 Opt.sayifDebug @@ "====> CaseAnalysis of EqNeq for " ^ (Entl.to_string_id ej);
             
             let _ejk = ref ej in
			 for k = 0 to L.length pppKj4 - 1 do
			   try
                 _ejk := Entl.clone ej;
				 !_ejk.Entl.name <- ej.Entl.name ^ "c" ^ (string_of_int k);
				 !_ejk.Entl.ant.SH.pi <- L.nth pppKj4 k;
				 Opt.sayifDebug @@ "Current subgoal:";
				 Opt.sayifDebug @@ Entl.to_string !_ejk;

				 Opt.sayifDebug0 @@ "Unsat check " ^ (Entl.to_string_id !_ejk) ^ "--> ";
				 if not(CcSatcheck.decideSatSH ddWand !_ejk.Entl.ant)
				 then
				   begin
					 Opt.sayifDebug "unsat (This branch succeeds)\n";
					 raise Skip (* Skip_A *)
				   end
				 else
				   begin
					 Opt.sayifDebug "sat\n";
					 ()
				   end;
					 
				 Opt.sayifDebug @@ "==> Match";
				 _entl := ccMatch !_ejk;
				 Opt.sayifDebug @@ "Match result:";
				 Opt.sayifDebug @@ Entl.to_string !_entl;

				 Opt.sayifDebug @@ "Simplify by checking root+cells";
				 _entl := reduceByCheckingDuplicatedRoot !_entl;
				 _entl := reduceByCompareRootCells !_entl;
				 Opt.sayifDebug @@ Entl.to_string !_entl;

				 Opt.sayifDebug @@ "Simplify by checking points-to";
				 _entl := reduceByCheckingPointsTo !_entl;
				 Opt.sayifDebug @@ Entl.to_string !_entl;
                 
				 Opt.sayifDebug0 @@ "Emp check " ^ (E.to_string_id !_entl) ^ "--> ";
				 begin
				   match ccEmpcheck !_entl with
				   | Some true ->
					  begin
						Opt.sayifDebug @@ "true (This branch succeeds)\n";
						raise Skip (* Skip_A *)
					  end
				   | Some false ->
					  begin
						Opt.sayifDebug @@ "false (This Branch fails)\n";
						raise BranchFailure
					  end
				   | None ->
					  begin
						Opt.sayifDebug @@ "LHS of the subgoal is not Emp";
						()
					  end
				 end;

				 Opt.sayifDebug0 @@ "Checking Right Emptiness " ^ (Entl.to_string_id !_entl) ^ "--> ";
				 if !_entl.Entl.suc = []
				 then
				   begin
					 Opt.sayifDebug @@ "empty (This branch fails)";
					 raise BranchFailure
				   end
				 else
				   begin
					 Opt.sayifDebug @@ "not empty";
					 ()
				   end;
				 
				 Opt.sayifDebug0 @@ "Identity checking ---> ";
				 if trivCheck !_entl
				 then
				   begin
					 Opt.sayifDebug "true (This branch is successfully finished)\n";
					 raise Skip (* Skip_A *)
				   end
				 else
				   begin
					 Opt.sayifDebug "false\n";
					 ()
				   end;
				 
				 Opt.sayifDebug @@ "==> Split PreProccessing";
				 let ge = ccSplitPreproc ddOrig ddWand !_uu !_yyWand contraIP dwand !_entl in
				 Opt.sayifDebug @@ "Split PreProccessing result:";
				 Opt.sayifDebug @@ GrpEntl.to_string ge;
                 
				 Opt.sayifDebug0 @@ "Checking Right Emptiness " ^ (GrpEntl.to_string_id ge) ^ "--> ";
				 if ge.GrpEntl.suc = []
				 then
				   begin
					 Opt.sayifDebug @@ "empty (This branch fails)";
					 raise BranchFailure
				   end
				 else
				   begin
					 Opt.sayifDebug @@ "not empty";
					 ()
				   end;
                 
				 Opt.sayifDebug0 @@ "Checking Single Group ---> ";
				 match GE.isSingleGroup ge with
				 | true ->
					begin
   					  Opt.sayifDebug @@ "true (It becomes the next subgoal)\n";
					  let e1 = ccNormalize (GE.fromSingleGroup ge) in
					  Opt.sayifDebug @@ (Entl.to_string_id e1) ^ " is pushed to the ControlStack\n";
					  _ctrlStack := Ctrl.Subgl(e1,[]) :: !_ctrlStack;
					end
				 | false ->
					begin
					  Opt.sayifDebug @@ "false\n";
					  let a1 = SB.mkInitState ge in
					  let b1 = SB.mkInitState ge in
					  let pos1 = SB.mkInitPos () in
                      let sub1 = SB.mkInitSub ge in
					  Opt.sayifDebug @@ "==> Subgoal " ^ (GrpEntl.to_string_id ge) ^ " is pushed to ControlStack\n";
					  _ctrlStack := Ctrl.Split(true,ge,sub1,a1,b1,pos1,!_H) :: !_ctrlStack;
					end

			   with Skip -> () (* Skip_A *)
			 done
		   done
		 end
	with
	| Skip -> () (* Skip_C *)
	| BranchFailure ->
	   begin
		 _branchFlag := false;
		 Opt.sayifDebug "===> Revert\n";
		 _ctrlStack := CS.revert !_ctrlStack;
	   end
  done;
;;


let ccMain (ps : PS.t) =
  let (e,ddOrig) = ps in
  let _result = ref true in
  let isEmpSH h = (L.length h.SH.gm = 1 && fst (L.hd h.SH.gm) = S.Emp) || L.length h.SH.gm = 0 in
  let _sginit = ref [] in
  let (_,h,hh) = Entl.decomp e in
  let ttFV = L.map var (SH.fv h) in
  let n = L.length h.SH.gm in
  let _initIndex = ref 0 in
  let _entlid = ref 0 in (* id number of entailments *)
  
  Opt.sayifDebug @@ Opt.debugPrompt ^ "Main start";

  let ttRoots = SH.getRoots h in
  let ttNonRoots = setminus ttFV ttRoots in
  let tttYXs = decompList ttNonRoots (n+1) in

  (* IndDefAnalysis を入れてみた部分 要検証
  let yyOrig = CcIndDefAnalysis.analyzeIS ddOrig in
  Opt.sayifDebug @@ "Indutive System analysis: \n" ^ (CcIndDefAnalysis.YY.to_string yyOrig) ^ "\n";
  let h0 = CcIndDefAnalysis.updateSH yyOrig h in
  let ttRootCells = SH.getRootCells h0 in
  let ttNonRootCells = setminus ttFV ttRootCells in
  let tttYXs = decompList ttNonRootCells (n+1) in
 *)  
  begin
  try
  for i = 0 to L.length tttYXs - 1 do
	let h' = SH.clone h in
	let ttYXs = L.nth tttYXs i in
	let ttY = L.hd ttYXs in
	let ttXs = L.tl ttYXs in
	h'.SH.gm <- L.map (fun ((g,zz),yy)->(g,dropRed (L.sort T.compare (zz@yy)))) (zipLst h'.SH.gm ttXs);
	h'.SH.up <- ttY;
  (* h1 becomes (up<y> & Pi & (P+<x1>)*(Q+<x2>)) *)
  (* when h is (Pi & P*Q) and ttYXs = [[[y],[x1],[x2]]] *)
	let ttX = ttRoots @ (L.flatten ttXs) in
	let _hh = ref [] in
	for j = 0 to L.length hh - 1 do
	  let hj = L.nth hh j in
	  let nj = L.length hj.SH.gm in
	  let ttNonRootsj = setminus ttX (SH.getRoots hj) in
	  let tttXj = decompList ttNonRootsj nj in
	  for k = 0 to L.length tttXj - 1 do
		let ttXj = L.nth tttXj k in
		let hj' = SH.clone hj in
		hj'.SH.gm <- L.map (fun ((g,zz),yy)->(g,dropRed(L.sort T.compare (zz@yy)))) (zipLst hj'.SH.gm ttXj);
		_hh := hj' :: !_hh;
	  done;
	done;
	let ttRoots = SH.getRoots h' in
	let ttOther = setminus ttFV ttRoots in
	let pppK = mkEqNeqAll2 ttRoots ttOther in
	for l = 0 to L.length pppK - 1 do
	  try
		let ppK = L.nth pppK l in
		let hl = SH.clone h' in
		hl.SH.pi <- PP.nf (ppK @ hl.SH.pi);
		let ttV = dropRed (L.flatten (L.map SH.getCells !_hh)) in
		let dwand = (IS.kmax ddOrig) + (L.length ttV) in
		let e1 = Entl.create (string_of_int !_entlid) hl !_hh in
        let nth = string_of_int l in
        let nthMax = string_of_int (L.length pppK - 1) in
		Opt.sayifDebug @@ "\nSubgoal [" ^ nth ^ "/" ^ nthMax ^ "] of UpDownCase:" ^ (string_of_int i) ^ " (produced by Main): ";
		Opt.sayifDebug @@ Entl.to_string e1;
        
		begin
          Opt.sayifDebug0 @@ "Left unsat check --> ";
		  if not(CcSatcheck.decideSatSH ddOrig e1.Entl.ant)
          then
            begin
              Opt.sayifDebug @@ "Unsat. Skip.";
              raise Skip
            end
          else
            begin
              Opt.sayifDebug @@ "Sat.";
              Opt.sayifDebug0 @@ "Left emptiness --> ";
		      match isEmpSH e1.Entl.ant with
			  | false ->
                 begin
                   Opt.sayifDebug @@ "false";
                   ()
                 end
			  | true ->
                 begin
                   Opt.sayifDebug @@ "true";
			       let pppl = L.map (fun h -> h.SH.pi) (L.filter isEmpSH !_hh) in
                   Opt.sayifDebug0 @@ "Checking pure-part --> ";
			       if PP.entlcheck e1.Entl.ant.SH.pi pppl
                   then
                     begin
                       Opt.sayifDebug @@ "true. Skip.";
                       raise Skip
                     end
                   else
                     begin
                       Opt.sayifDebug @@ "false. return Invalid.";
                       raise Invalid
                     end
                 end
            end
		end;
        
		Opt.sayifDebug @@ "Simplify (left-Eq elimination)";
		let e2 = elimLeftEq e1 in
		Opt.sayifDebug @@ Entl.to_string e2;

		Opt.sayifDebug @@ "Simplify (right allocated Emp elimination)";
        let e3 = elimRightAllocatedEmp e2 in
        Opt.sayifDebug @@ Entl.to_string e3;
        
		ccMainLoop ddOrig dwand (e3,[!_initIndex]);

		_entlid := !_entlid + 1;
		_initIndex := !_initIndex + 1;
	  with
      | MainLoopFail -> raise Invalid
      | Skip -> ()
	done;
  done;
  with Invalid -> _result := false
  end;
  !_result
;;
