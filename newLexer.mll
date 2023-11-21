(* Lexer *)

{
open NewParser
}

let space = [' ' '\t' '\n' '\r']
let eol = ['\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''
let comment = "#"
let indefsym = "---"
  
rule token = parse
  | '*'       { AST }
  | "w*"      { WAST }
  | '='       { EQ }
  | "=/"      { NEQ }
  | "<>"      { NEQ }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | ','       { COMMA }
  | '@'       { ATMARK }
  | '/'       { SLASH }
  | '^'       { HAT }

  | "Nil"     { NIL }
  | "Emp"     { EMP }
  | "&&"      { ANDAND }
  | '&'       { AND }
  | '|'       { OR }
  | "->"      { PTO }
  | "|-"      { VDASH }

  | digit
    { DIG (int_of_string(Lexing.lexeme lexbuf)) } 

  | alpha alnum*
    { IDENT (Lexing.lexeme lexbuf) }

  | eof       { EOF }

  | space+    { token lexbuf }

  | comment [^ '\n' '\r']* eol { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }
