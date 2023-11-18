
module Satchecker = struct

  type t =
	{
	  mutable debug : bool; (* flag for debug mode *)
	  mutable model : bool; (* flag for model-generation mode*)
	}

  let o =
	{
	  debug = false;
	  model = false;
	}

  let debugFlag () = o.debug
  let modelFlag () = o.model
	  
  let setDebug () = o.debug <- true
  let unsetDebug () = o.debug <- false
	
  let setModel () = o.model <- true
  let unsetModel () = o.model <- false

  let doifDebug f arg = if o.debug then f arg else ()
                      
  let sayifDebug mes = 
	let f () = print_endline mes in
	doifDebug f ()

  let sayifDebug0 mes =
	let f () = print_string mes in
	doifDebug f ()
                      
end
;;

module Cycomp = struct

  type t =
	{
	  mutable debug : bool; (* flag for debug mode *)
      mutable optimization : bool; (* do factor analysis *)
      mutable subsetsplit : bool; (* do subset-split case analysis *)
	}

  let o =
	{
	  debug = false;
      optimization = false;
      subsetsplit = false;
	}

  let debugFlag () = o.debug
	  
  let setDebug () = o.debug <- true
  let unsetDebug () = o.debug <- false
	
  let doifDebug f arg = if o.debug then f arg else ()
	  
  let sayifDebug mes = 
	let f () = print_endline mes in
	doifDebug f ()

  let sayifDebug0 mes =
	let f () = print_string mes in
	doifDebug f ()
	  
  let debugPrompt = "==> "

  let setOpt () = o.optimization <- true

  let setFull () = o.subsetsplit <- true                
                
  let doifOpt f = if o.optimization then f () else ()

  let doifNotFull f = if not(o.subsetsplit) then f () else ()
    
end
;;

