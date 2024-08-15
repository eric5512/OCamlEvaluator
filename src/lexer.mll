{
  open Parser

  exception Error of string

}

rule line = parse
| ([^'\n']* '\n') as line
    { Some line, true }
| eof
    { None, false }
| ([^'\n']+ as line) eof
    { Some (line ^ "\n"), false }

and token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOL }
| "DEF"
    { DEF }
| "DER"
    { DER }
| "SIM"
    { SIM }
| ['0'-'9']+ '.'? ['0'-'9']* as f
    { NUM (float_of_string f) }
| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as s 
    { ID s }
| ":="
    { ASSIGN }
| '+'
    { ADD }
| '-'
    { SUB }
| '^'
    { POW }
| '*'
    { MUL }
| '/'
    { DIV }
| '('
    { LPAR }
| ')'
    { RPAR }
| ','
    { COMMA }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }