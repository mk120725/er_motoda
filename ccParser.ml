type token =
  | IDENT of (string)
  | AST
  | EQ
  | NEQ
  | LPAREN
  | RPAREN
  | COMMA
  | NIL
  | EMP
  | EX
  | OUT
  | UP
  | DOWN
  | AND
  | OR
  | PTO
  | VDASH
  | SEP
  | DEFEQ
  | ANDAND
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "ccParser.mly"
open CcSyntax
# 29 "ccParser.ml"
let yytransl_const = [|
  258 (* AST *);
  259 (* EQ *);
  260 (* NEQ *);
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* COMMA *);
  264 (* NIL *);
  265 (* EMP *);
  266 (* EX *);
  267 (* OUT *);
  268 (* UP *);
  269 (* DOWN *);
  270 (* AND *);
  271 (* OR *);
  272 (* PTO *);
  273 (* VDASH *);
  274 (* SEP *);
  275 (* DEFEQ *);
  276 (* ANDAND *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\014\000\014\000\014\000\014\000\004\000\004\000\005\000\005\000\
\005\000\006\000\006\000\006\000\006\000\007\000\007\000\007\000\
\008\000\008\000\015\000\016\000\017\000\017\000\018\000\018\000\
\018\000\018\000\018\000\009\000\009\000\019\000\020\000\020\000\
\020\000\011\000\012\000\012\000\013\000\021\000\021\000\010\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\001\000\001\000\003\000\001\000\
\001\000\003\000\003\000\001\000\003\000\003\000\001\000\003\000\
\003\000\001\000\003\000\004\000\003\000\001\000\004\000\004\000\
\001\000\003\000\002\000\002\000\001\000\003\000\001\000\003\000\
\003\000\005\000\005\000\001\000\005\000\004\000\001\000\002\000\
\003\000\003\000\001\000\002\000\002\000\001\000\003\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\000\000\000\000\006\000\018\000\000\000\
\000\000\000\000\000\000\049\000\000\000\015\000\000\000\000\000\
\025\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
\036\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
\008\000\005\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\
\000\000\001\000\000\000\000\000\000\000\007\000\017\000\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\014\000\000\000\000\000\016\000\000\000\000\000\000\000\
\000\000\026\000\046\000\000\000\000\000\044\000\000\000\000\000\
\032\000\000\000\033\000\020\000\004\000\000\000\003\000\011\000\
\010\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000\047\000\000\000\000\000\039\000\000\000\
\034\000\035\000\038\000\040\000\000\000\041\000"

let yydgoto = "\002\000\
\012\000\032\000\013\000\014\000\015\000\016\000\017\000\018\000\
\019\000\020\000\078\000\049\000\021\000\037\000\022\000\023\000\
\024\000\025\000\079\000\104\000\076\000"

let yysindex = "\012\000\
\090\255\000\000\000\000\018\255\154\255\000\000\000\000\041\255\
\043\255\043\255\043\255\000\000\050\255\000\000\253\254\020\255\
\000\000\068\255\028\255\055\255\077\000\073\255\076\255\000\000\
\000\000\043\255\046\255\012\255\082\255\000\000\041\255\087\255\
\000\000\000\000\043\255\000\000\101\255\101\255\057\255\106\255\
\106\255\043\255\106\255\019\255\084\255\019\255\090\255\000\000\
\109\255\000\000\118\255\132\255\114\255\000\000\000\000\000\000\
\146\255\104\255\121\255\119\255\150\255\106\255\164\255\106\255\
\000\000\000\000\101\255\077\255\000\000\164\255\108\255\068\255\
\043\255\000\000\000\000\113\255\125\255\000\000\117\255\126\255\
\000\000\130\255\000\000\000\000\000\000\152\255\000\000\000\000\
\000\000\000\000\119\255\006\255\101\255\090\255\041\255\074\255\
\134\255\134\255\000\000\000\000\160\255\090\255\000\000\123\255\
\000\000\000\000\000\000\000\000\090\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\145\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\000\000\029\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\140\255\148\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\170\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\167\255\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\033\000\
\000\000\000\000\000\000\055\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\233\255\251\255\128\000\171\000\253\255\129\000\133\000\
\223\255\000\000\000\000\000\000\000\000\005\000\126\000\130\000\
\214\255\121\000\000\000\000\000\000\000"

let yytablesize = 307
let yytable = "\027\000\
\019\000\029\000\042\000\036\000\036\000\036\000\022\000\057\000\
\081\000\083\000\043\000\054\000\001\000\075\000\038\000\039\000\
\044\000\055\000\003\000\004\000\036\000\042\000\026\000\070\000\
\023\000\043\000\006\000\007\000\029\000\060\000\053\000\011\000\
\030\000\045\000\065\000\066\000\036\000\068\000\071\000\061\000\
\071\000\030\000\033\000\034\000\047\000\031\000\067\000\035\000\
\040\000\041\000\006\000\054\000\040\000\041\000\105\000\106\000\
\089\000\071\000\091\000\090\000\100\000\042\000\103\000\062\000\
\092\000\042\000\029\000\036\000\108\000\046\000\063\000\101\000\
\048\000\003\000\004\000\110\000\050\000\093\000\005\000\040\000\
\041\000\006\000\007\000\008\000\009\000\010\000\011\000\056\000\
\102\000\003\000\004\000\058\000\051\000\059\000\005\000\052\000\
\073\000\006\000\007\000\008\000\009\000\010\000\011\000\003\000\
\004\000\003\000\034\000\062\000\005\000\077\000\064\000\006\000\
\007\000\006\000\009\000\010\000\011\000\003\000\004\000\084\000\
\062\000\087\000\005\000\042\000\054\000\006\000\007\000\094\000\
\009\000\095\000\011\000\003\000\004\000\003\000\004\000\096\000\
\005\000\109\000\005\000\006\000\007\000\006\000\007\000\010\000\
\011\000\097\000\011\000\005\000\005\000\098\000\005\000\085\000\
\059\000\003\000\004\000\088\000\062\000\099\000\005\000\028\000\
\005\000\006\000\007\000\003\000\004\000\107\000\059\000\027\000\
\070\000\045\000\069\000\006\000\007\000\009\000\074\000\028\000\
\072\000\082\000\086\000\000\000\080\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\019\000\042\000\000\000\000\000\019\000\022\000\
\022\000\000\000\000\000\000\000\022\000\000\000\019\000\019\000\
\000\000\019\000\019\000\000\000\000\000\022\000\000\000\022\000\
\022\000\023\000\023\000\000\000\000\000\029\000\023\000\000\000\
\000\000\030\000\029\000\000\000\000\000\000\000\030\000\023\000\
\000\000\023\000\023\000\029\000\000\000\029\000\029\000\030\000\
\000\000\030\000\030\000"

let yycheck = "\005\000\
\000\000\005\000\000\000\009\000\010\000\011\000\000\000\031\000\
\051\000\052\000\014\001\006\001\001\000\047\000\010\000\011\000\
\020\001\006\001\000\001\001\001\026\000\016\001\005\001\005\001\
\000\000\014\001\008\001\009\001\000\000\035\000\026\000\013\001\
\000\000\014\001\040\000\041\000\042\000\043\000\044\000\035\000\
\046\000\001\001\000\001\001\001\017\001\005\001\042\000\005\001\
\003\001\004\001\008\001\006\001\003\001\004\001\097\000\098\000\
\062\000\063\000\064\000\063\000\094\000\016\001\096\000\007\001\
\070\000\016\001\070\000\073\000\102\000\002\001\014\001\095\000\
\018\001\000\001\001\001\109\000\000\000\073\000\005\001\003\001\
\004\001\008\001\009\001\010\001\011\001\012\001\013\001\006\001\
\015\001\000\001\001\001\005\001\020\001\007\001\005\001\020\001\
\013\001\008\001\009\001\010\001\011\001\012\001\013\001\000\001\
\001\001\000\001\001\001\007\001\005\001\001\001\005\001\008\001\
\009\001\008\001\011\001\012\001\013\001\000\001\001\001\006\001\
\007\001\001\001\005\001\016\001\006\001\008\001\009\001\015\001\
\011\001\005\001\013\001\000\001\001\001\000\001\001\001\019\001\
\005\001\015\001\005\001\008\001\009\001\008\001\009\001\012\001\
\013\001\020\001\013\001\003\001\004\001\020\001\006\001\006\001\
\007\001\000\001\001\001\006\001\007\001\006\001\005\001\020\001\
\016\001\008\001\009\001\000\001\001\001\006\001\007\001\020\001\
\005\001\000\000\043\000\008\001\009\001\007\001\046\000\005\000\
\044\000\052\000\058\000\255\255\051\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\001\001\255\255\255\255\006\001\001\001\
\002\001\255\255\255\255\255\255\006\001\255\255\014\001\015\001\
\255\255\017\001\018\001\255\255\255\255\015\001\255\255\017\001\
\018\001\001\001\002\001\255\255\255\255\001\001\006\001\255\255\
\255\255\001\001\006\001\255\255\255\255\255\255\006\001\015\001\
\255\255\017\001\018\001\015\001\255\255\017\001\018\001\015\001\
\255\255\017\001\018\001"

let yynames_const = "\
  AST\000\
  EQ\000\
  NEQ\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  NIL\000\
  EMP\000\
  EX\000\
  OUT\000\
  UP\000\
  DOWN\000\
  AND\000\
  OR\000\
  PTO\000\
  VDASH\000\
  SEP\000\
  DEFEQ\000\
  ANDAND\000\
  EOF\000\
  "

let yynames_block = "\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : ProgSys.t) in
    Obj.repr(
# 62 "ccParser.mly"
     ( _1 )
# 250 "ccParser.ml"
               : CcSyntax.ProgSys.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "ccParser.mly"
      ( [_1] )
# 257 "ccParser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "ccParser.mly"
      ( _1 @ [_3] )
# 265 "ccParser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string list) in
    Obj.repr(
# 71 "ccParser.mly"
   ( _2 )
# 272 "ccParser.ml"
               : string list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "ccParser.mly"
    ( SHterm.Var _1 )
# 279 "ccParser.ml"
               : T.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "ccParser.mly"
    ( SHterm.Nil )
# 285 "ccParser.ml"
               : T.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : T.t) in
    Obj.repr(
# 80 "ccParser.mly"
      ( _2 )
# 292 "ccParser.ml"
               : T.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "ccParser.mly"
    ( 
      let message =
        Printf.sprintf 
          "parse error at line %d:%d-%d"
          ((Parsing.symbol_start_pos ()).Lexing.pos_lnum)
		  (Parsing.symbol_start ())
		  (Parsing.symbol_end ())
	    in
	    failwith message
    )
# 307 "ccParser.ml"
               : T.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : T.t) in
    Obj.repr(
# 96 "ccParser.mly"
      ( [_1] )
# 314 "ccParser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term_seq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : T.t) in
    Obj.repr(
# 99 "ccParser.mly"
      ( _1 @ [_3] )
# 322 "ccParser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term_seq) in
    Obj.repr(
# 102 "ccParser.mly"
   ( _2 )
# 329 "ccParser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "ccParser.mly"
    ( 
      let message =
        Printf.sprintf 
          "parse error (term_seq) near characters %d-%d"
          (Parsing.symbol_start ())
	      (Parsing.symbol_end ())
	    in
	    failwith message
    )
# 343 "ccParser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : T.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : T.t) in
    Obj.repr(
# 118 "ccParser.mly"
    ( (Eq,_1,_3) )
# 351 "ccParser.ml"
               : P.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : T.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : T.t) in
    Obj.repr(
# 121 "ccParser.mly"
    ( (Neq,_1,_3) )
# 359 "ccParser.ml"
               : P.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : P.t) in
    Obj.repr(
# 126 "ccParser.mly"
   ( [_1] )
# 366 "ccParser.ml"
               : PP.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : PP.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : P.t) in
    Obj.repr(
# 128 "ccParser.mly"
   ( _1 @ [_3] )
# 374 "ccParser.ml"
               : PP.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : PP.t) in
    Obj.repr(
# 130 "ccParser.mly"
 ( _2 )
# 381 "ccParser.ml"
               : PP.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "ccParser.mly"
   ( SHspatExp.Emp )
# 387 "ccParser.ml"
               : S.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : T.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_seq) in
    Obj.repr(
# 137 "ccParser.mly"
     ( SHspatExp.Pto(_1,_3) )
# 395 "ccParser.ml"
               : S.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_seq) in
    Obj.repr(
# 139 "ccParser.mly"
      ( SHspatExp.Pr(_1,_3) )
# 403 "ccParser.ml"
               : S.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : S.t) in
    Obj.repr(
# 141 "ccParser.mly"
   ( _2 )
# 410 "ccParser.ml"
               : S.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : S.t) in
    Obj.repr(
# 146 "ccParser.mly"
   ( (_1,[]) )
# 417 "ccParser.ml"
               : G.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : S.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'term_seq) in
    Obj.repr(
# 148 "ccParser.mly"
          ( (_1,_4) )
# 425 "ccParser.ml"
               : G.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'term_seq) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : S.t) in
    Obj.repr(
# 150 "ccParser.mly"
          ( (_4,_2) )
# 433 "ccParser.ml"
               : G.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : G.t) in
    Obj.repr(
# 155 "ccParser.mly"
   ( [_1] )
# 440 "ccParser.ml"
               : GG.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : GG.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : G.t) in
    Obj.repr(
# 157 "ccParser.mly"
   ( _1 @ [_3] )
# 448 "ccParser.ml"
               : GG.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term_seq) in
    Obj.repr(
# 162 "ccParser.mly"
   ( _2 )
# 455 "ccParser.ml"
               : 'sh_up))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term_seq) in
    Obj.repr(
# 167 "ccParser.mly"
   ( _2 )
# 462 "ccParser.ml"
               : 'sh_out))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : GG.t) in
    Obj.repr(
# 172 "ccParser.mly"
      ( ([],_1) )
# 469 "ccParser.ml"
               : 'sh_core))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : PP.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : GG.t) in
    Obj.repr(
# 174 "ccParser.mly"
   ( (_1,_3) )
# 477 "ccParser.ml"
               : 'sh_core))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sh_core) in
    Obj.repr(
# 179 "ccParser.mly"
   ( let (pi,gm) = _1 in ([],[],pi,gm) )
# 484 "ccParser.ml"
               : 'sh_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sh_up) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sh_core) in
    Obj.repr(
# 182 "ccParser.mly"
   ( let (pi,gm) = _3 in (_1,[],pi,gm) )
# 492 "ccParser.ml"
               : 'sh_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sh_out) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sh_core) in
    Obj.repr(
# 185 "ccParser.mly"
   (	let (pi,gm) = _3 in ([],_1,pi,gm) )
# 500 "ccParser.ml"
               : 'sh_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'sh_up) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'sh_out) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sh_core) in
    Obj.repr(
# 188 "ccParser.mly"
   (	let (pi,gm) = _5 in (_1,_3,pi,gm) )
# 509 "ccParser.ml"
               : 'sh_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'sh_out) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'sh_up) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'sh_core) in
    Obj.repr(
# 191 "ccParser.mly"
   (	let (pi,gm) = _5 in (_3,_1,pi,gm) )
# 518 "ccParser.ml"
               : 'sh_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sh_body) in
    Obj.repr(
# 196 "ccParser.mly"
      (
		let (up1,out1,pi1,gm1) = _1 in
		{ SH.ex=[]; SH.up=up1; SH.out=out1; SH.pi=pi1; SH.gm=gm1 }
	  )
# 528 "ccParser.ml"
               : SH.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'sh_body) in
    Obj.repr(
# 202 "ccParser.mly"
   (
		let (up1,out1,pi1,gm1) = _4 in
		{ SH.ex=_2; SH.up=up1; SH.out=out1; SH.pi=pi1; SH.gm=gm1 }
	  )
# 539 "ccParser.ml"
               : SH.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string list) in
    Obj.repr(
# 210 "ccParser.mly"
   ( (_1,_3) )
# 547 "ccParser.ml"
               : 'def_head))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : SH.t) in
    Obj.repr(
# 215 "ccParser.mly"
   ( [_1] )
# 554 "ccParser.ml"
               : 'def_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : SH.t) in
    Obj.repr(
# 217 "ccParser.mly"
      ( [_2] )
# 561 "ccParser.ml"
               : 'def_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def_body) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : SH.t) in
    Obj.repr(
# 219 "ccParser.mly"
   ( _1 @ [_3] )
# 569 "ccParser.ml"
               : 'def_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def_head) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'def_body) in
    Obj.repr(
# 224 "ccParser.mly"
   (
		let (pr,prm) = _1 in
		(pr,prm,_3)
	  )
# 580 "ccParser.ml"
               : IndDef.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 232 "ccParser.mly"
   ( [] )
# 586 "ccParser.ml"
               : IndSys.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : IndSys.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : IndDef.t) in
    Obj.repr(
# 235 "ccParser.mly"
   ( _1 @ [_2] )
# 594 "ccParser.ml"
               : IndSys.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Entl.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : IndSys.t) in
    Obj.repr(
# 240 "ccParser.mly"
   ( (_1,_2) )
# 602 "ccParser.ml"
               : ProgSys.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : SH.t) in
    Obj.repr(
# 245 "ccParser.mly"
   ( [_1] )
# 609 "ccParser.ml"
               : 'entl_right))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'entl_right) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : SH.t) in
    Obj.repr(
# 248 "ccParser.mly"
   ( _1 @ [_3] )
# 617 "ccParser.ml"
               : 'entl_right))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : SH.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'entl_right) in
    Obj.repr(
# 253 "ccParser.mly"
   ( { Entl.name=""; Entl.ant=_1; Entl.suc=_3 } )
# 625 "ccParser.ml"
               : Entl.t))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : CcSyntax.ProgSys.t)
