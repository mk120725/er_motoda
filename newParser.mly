// Parser for CyComp 2018/05/13

%{
open Ratio
open NewSyntax
%}

%token <string> IDENT  // x, y, P, ...
%token <int> DIG

%token AST	// '*'
%token WAST	// "w*"
%token EQ	// '='
%token NEQ	// "=/"
%token ATMARK	// "@"
%token SLASH	// "/"
%token HAT	// "^"

%token LPAREN   // '('
%token RPAREN   // ')'
%token COMMA    // ','

%token NIL      // "Nil"
%token EMP      // "Emp"
%token EX	// "Ex"
%token OUT	// "Out"
%token UP	// "Up"
%token DOWN	// "Dn"
%token AND      // '&'
%token OR       // '|'
%token PTO	// "->"
%token VDASH    // "|-"
%token SEP      // "---"
%token DEFEQ	// ":="
%token ANDAND	// "&&"

%token EOF 

// 結合力(優先度が低い順)
%nonassoc VDASH
%nonassoc EX
%nonassoc COLON
%left AND OR
%left EQ NEQ
%left AST
%left WAST
%nonassoc PTO UP DOWN OUT
%left VAR NIL EMP LPAREN

%start main
%type <NewSyntax.Entl.t> main
%type <SHterm.t> term
%type <SHpureExp.t> pure_exp
%type <SHpure.t> pure
%type <SHspatExp.t> spat_exp
%type <SHspat.t> spat
%type <SH.t> sh
%%

// 
main:
  | sh VDASH sh EOF
     { { Entl.up=[]; Entl.ant=$1; Entl.suc=$3 } }
  ;

  
term:
  | IDENT
    { SHterm.Var $1 }
  | NIL
    { SHterm.Nil }
  | LPAREN term RPAREN
      { $2 }
  | error
    { 
      let message =
        Printf.sprintf 
          "parse error at line %d:%d-%d"
          ((Parsing.symbol_start_pos ()).Lexing.pos_lnum)
		  (Parsing.symbol_start ())
		  (Parsing.symbol_end ())
	    in
	    failwith message
    }	  
;

term_seq:
  | term
      { [$1] }
  | term_seq COMMA term
      { $1 @ [$3] }
  | LPAREN term_seq RPAREN
	  { $2 }
  | error
    { 
      let message =
        Printf.sprintf 
          "parse error (term_seq) near characters %d-%d"
          (Parsing.symbol_start ())
	      (Parsing.symbol_end ())
	    in
	    failwith message
    }	  	  
;

pure_exp:
  | term EQ term
    { SHpureExp.Eq($1,$3) }
  | term NEQ term
    { SHpureExp.NEq($1,$3) }
  | ATMARK LPAREN IDENT COMMA spat RPAREN   // @ (label, spatial)
      { SHpureExp.At($3,$5) }
;

pure:
  | pure_exp
	  { [$1] }
  | pure AND pure_exp
	  { $1 @ [$3] }
  | LPAREN pure RPAREN
	{ $2 }
;

spat_exp:
  | EMP					// Emp
	  { SHspatExp.Emp }
  | term PTO term_seq			// t -> (t,..,t)
     { SHspatExp.Pto($1,$3) }
  | IDENT LPAREN term_seq RPAREN	// P ( t,..,t )
      { SHspatExp.Pr($1,$3) }
  | IDENT HAT LPAREN DIG SLASH DIG RPAREN // a ^ ( 1 / 2 )
      { SHspatExp.Lab($1, Ratio.make_ratio $4 $6) }
  | LPAREN spat_exp RPAREN
      { $2 }
;

spat:
  | spat_exp
	  { SHspat.SAtom($1) }
  | spat AST spat   // sigma1 * sigma2
	  { SHspat.SCon($1,$3) }
  | spat WAST spat   // sigma1 w* sigma2
	  { SHspat.WCon($1,$3) }
  | LPAREN spat RPAREN
      { $2 }
;

sh:
  | spat
      { ([],$1) }
  | pure ANDAND spat
	  { ($1,$3) }
;  

