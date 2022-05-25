(* Ocamllex scanner for HANDL *)

{ open Handlparse }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ascii = [' '-'~']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVISION }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| ">="     { GEQ }
| "<="     { LEQ }
| '>'      { GT }
| "and"    { AND }
| "or"     { OR }
| "not"    { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "Array"  { ARRAY }
| "String" { STRING }
| "for" { FOR }
| "Note" { NOTE }
| "Phrase" { PHRASE }
| ".add" { ADDNOTE }
| ".play()" { PLAY }
| ".measure" { MEASURE }
| "Song" { SONG }
| "new" { NEW }
| '"'((ascii)* as str)'"'  { STRLIT(str) }
| digit+ as lem  { LITERAL(int_of_string lem) }
| digit+ '.' digit+ ( ['e' 'E'] ['+' '-']? digit+ )? as lem {FLIT(float_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
