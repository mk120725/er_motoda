type token =
  | IDENT of (string)
  | DIG of (int)
  | AST
  | WAST
  | EQ
  | NEQ
  | ATMARK
  | SLASH
  | HAT
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
# 4 "newParser.mly"
open Ratio
open NewSyntax
# 35 "newParser.ml"
let yytransl_const = [|
  259 (* AST *);
  260 (* WAST *);
  261 (* EQ *);
  262 (* NEQ *);
  263 (* ATMARK *);
  264 (* SLASH *);
  265 (* HAT *);
  266 (* LPAREN *);
  267 (* RPAREN *);
  268 (* COMMA *);
  269 (* NIL *);
  270 (* EMP *);
  271 (* EX *);
  272 (* OUT *);
  273 (* UP *);
  274 (* DOWN *);
  275 (* AND *);
  276 (* OR *);
  277 (* PTO *);
  278 (* VDASH *);
  279 (* SEP *);
  280 (* DEFEQ *);
  281 (* ANDAND *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* DIG *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\008\000\008\000\008\000\
\008\000\003\000\003\000\003\000\004\000\004\000\004\000\005\000\
\005\000\005\000\005\000\005\000\006\000\006\000\006\000\006\000\
\007\000\007\000\000\000"

let yylen = "\002\000\
\004\000\001\000\001\000\003\000\001\000\001\000\003\000\003\000\
\001\000\003\000\003\000\006\000\001\000\003\000\003\000\001\000\
\003\000\004\000\007\000\003\000\001\000\003\000\003\000\003\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\005\000\000\000\000\000\000\000\003\000\016\000\
\027\000\000\000\013\000\000\000\021\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\002\000\000\000\006\000\000\000\000\000\004\000\015\000\020\000\
\024\000\000\000\010\000\011\000\000\000\000\000\014\000\000\000\
\000\000\000\000\000\000\023\000\000\000\000\000\000\000\000\000\
\018\000\000\000\000\000\000\000\000\000\001\000\000\000\008\000\
\007\000\000\000\000\000\012\000\019\000"

let yydgoto = "\002\000\
\009\000\049\000\011\000\012\000\013\000\014\000\015\000\036\000"

let yysindex = "\027\000\
\049\255\000\000\000\000\034\255\030\255\049\255\000\000\000\000\
\000\000\033\255\000\000\243\254\000\000\068\255\024\255\038\255\
\008\255\057\255\009\255\026\255\020\255\072\255\069\255\069\255\
\008\255\067\255\051\255\051\255\051\255\049\255\058\255\000\000\
\000\000\008\255\000\000\075\255\061\255\000\000\000\000\000\000\
\000\000\069\255\000\000\000\000\066\255\083\255\000\000\051\255\
\060\255\068\255\091\255\000\000\096\000\089\255\087\255\079\255\
\000\000\069\255\051\255\087\255\252\254\000\000\097\255\000\000\
\000\000\081\255\092\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\021\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\090\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\004\000\003\000\000\000\000\000\000\000\088\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\075\000\098\000\005\000\007\000\072\000\241\255"

let yytablesize = 282
let yytable = "\010\000\
\017\000\025\000\022\000\026\000\019\000\026\000\038\000\032\000\
\033\000\045\000\021\000\027\000\022\000\023\000\024\000\035\000\
\025\000\034\000\056\000\038\000\007\000\043\000\044\000\035\000\
\046\000\002\000\002\000\001\000\010\000\025\000\040\000\002\000\
\055\000\050\000\051\000\052\000\039\000\023\000\024\000\018\000\
\060\000\002\000\016\000\017\000\026\000\030\000\061\000\031\000\
\003\000\004\000\003\000\004\000\021\000\025\000\022\000\005\000\
\065\000\037\000\006\000\054\000\048\000\007\000\008\000\007\000\
\008\000\066\000\003\000\033\000\003\000\033\000\028\000\029\000\
\059\000\005\000\028\000\029\000\042\000\058\000\042\000\007\000\
\025\000\007\000\041\000\028\000\029\000\057\000\058\000\023\000\
\024\000\064\000\058\000\068\000\021\000\021\000\029\000\062\000\
\063\000\038\000\067\000\006\000\047\000\053\000\069\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\017\000\017\000\022\000\000\000\000\000\
\000\000\000\000\000\000\017\000\000\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\017\000\025\000\
\022\000\026\000"

let yycheck = "\001\000\
\000\000\000\000\000\000\000\000\006\000\019\001\011\001\000\001\
\001\001\025\000\006\000\025\001\006\000\005\001\006\001\017\000\
\021\001\010\001\034\000\011\001\013\001\023\000\024\000\025\000\
\026\000\005\001\006\001\001\000\030\000\021\001\011\001\011\001\
\034\000\027\000\028\000\029\000\011\001\005\001\006\001\010\001\
\042\000\021\001\009\001\010\001\019\001\022\001\048\000\010\001\
\000\001\001\001\000\001\001\001\048\000\021\001\048\000\007\001\
\058\000\001\001\010\001\002\001\010\001\013\001\014\001\013\001\
\014\001\059\000\000\001\001\001\000\001\001\001\003\001\004\001\
\012\001\007\001\003\001\004\001\010\001\012\001\010\001\013\001\
\021\001\013\001\011\001\003\001\004\001\011\001\012\001\005\001\
\006\001\011\001\012\001\011\001\003\001\004\001\004\001\000\000\
\008\001\011\001\002\001\012\001\026\000\030\000\011\001\006\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\003\001\255\255\255\255\
\255\255\255\255\255\255\011\001\255\255\011\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\022\001\022\001\
\022\001\022\001"

let yynames_const = "\
  AST\000\
  WAST\000\
  EQ\000\
  NEQ\000\
  ATMARK\000\
  SLASH\000\
  HAT\000\
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
  DIG\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : SH.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : SH.t) in
    Obj.repr(
# 63 "newParser.mly"
     ( { Entl.up=[]; Entl.ant=_1; Entl.suc=_3 } )
# 236 "newParser.ml"
               : NewSyntax.Entl.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "newParser.mly"
    ( SHterm.Var _1 )
# 243 "newParser.ml"
               : SHterm.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "newParser.mly"
    ( SHterm.Nil )
# 249 "newParser.ml"
               : SHterm.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : SHterm.t) in
    Obj.repr(
# 73 "newParser.mly"
      ( _2 )
# 256 "newParser.ml"
               : SHterm.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "newParser.mly"
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
# 271 "newParser.ml"
               : SHterm.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : SHterm.t) in
    Obj.repr(
# 89 "newParser.mly"
      ( [_1] )
# 278 "newParser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term_seq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : SHterm.t) in
    Obj.repr(
# 91 "newParser.mly"
      ( _1 @ [_3] )
# 286 "newParser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term_seq) in
    Obj.repr(
# 93 "newParser.mly"
   ( _2 )
# 293 "newParser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "newParser.mly"
    ( 
      let message =
        Printf.sprintf 
          "parse error (term_seq) near characters %d-%d"
          (Parsing.symbol_start ())
	      (Parsing.symbol_end ())
	    in
	    failwith message
    )
# 307 "newParser.ml"
               : 'term_seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : SHterm.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : SHterm.t) in
    Obj.repr(
# 108 "newParser.mly"
    ( SHpureExp.Eq(_1,_3) )
# 315 "newParser.ml"
               : SHpureExp.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : SHterm.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : SHterm.t) in
    Obj.repr(
# 110 "newParser.mly"
    ( SHpureExp.NEq(_1,_3) )
# 323 "newParser.ml"
               : SHpureExp.t))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : SHspat.t) in
    Obj.repr(
# 112 "newParser.mly"
      ( SHpureExp.At(_3,_5) )
# 331 "newParser.ml"
               : SHpureExp.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : SHpureExp.t) in
    Obj.repr(
# 117 "newParser.mly"
   ( [_1] )
# 338 "newParser.ml"
               : SHpure.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : SHpure.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : SHpureExp.t) in
    Obj.repr(
# 119 "newParser.mly"
   ( _1 @ [_3] )
# 346 "newParser.ml"
               : SHpure.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : SHpure.t) in
    Obj.repr(
# 121 "newParser.mly"
 ( _2 )
# 353 "newParser.ml"
               : SHpure.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "newParser.mly"
   ( SHspatExp.Emp )
# 359 "newParser.ml"
               : SHspatExp.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : SHterm.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_seq) in
    Obj.repr(
# 128 "newParser.mly"
     ( SHspatExp.Pto(_1,_3) )
# 367 "newParser.ml"
               : SHspatExp.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_seq) in
    Obj.repr(
# 130 "newParser.mly"
      ( SHspatExp.Pr(_1,_3) )
# 375 "newParser.ml"
               : SHspatExp.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 132 "newParser.mly"
      ( SHspatExp.Lab(_1, Ratio.make_ratio _4 _6) )
# 384 "newParser.ml"
               : SHspatExp.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : SHspatExp.t) in
    Obj.repr(
# 134 "newParser.mly"
      ( _2 )
# 391 "newParser.ml"
               : SHspatExp.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : SHspatExp.t) in
    Obj.repr(
# 139 "newParser.mly"
   ( SHspat.SAtom(_1) )
# 398 "newParser.ml"
               : SHspat.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : SHspat.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : SHspat.t) in
    Obj.repr(
# 141 "newParser.mly"
   ( SHspat.SCon(_1,_3) )
# 406 "newParser.ml"
               : SHspat.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : SHspat.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : SHspat.t) in
    Obj.repr(
# 143 "newParser.mly"
   ( SHspat.WCon(_1,_3) )
# 414 "newParser.ml"
               : SHspat.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : SHspat.t) in
    Obj.repr(
# 145 "newParser.mly"
      ( _2 )
# 421 "newParser.ml"
               : SHspat.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : SHspat.t) in
    Obj.repr(
# 150 "newParser.mly"
      ( ([],_1) )
# 428 "newParser.ml"
               : SH.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : SHpure.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : SHspat.t) in
    Obj.repr(
# 152 "newParser.mly"
   ( (_1,_3) )
# 436 "newParser.ml"
               : SH.t))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : NewSyntax.Entl.t)
