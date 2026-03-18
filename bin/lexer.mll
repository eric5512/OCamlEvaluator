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
| "CONV"
    { CONV }
| "BASE"
    { BASE }
| "SOLVE"
    { SOLVE }
| "PLOT"
    { PLOT }
| "LIST"
    { LIST }
| "HELP"
    { HELP }
| "0b" ['0' '1']+ as n
    { NUM (float_of_int (int_of_string n)) }
| "0o" ['0'-'7']+ as n
    { NUM (float_of_int (int_of_string n)) }
| "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ as n
    { NUM (float_of_int (int_of_string n)) }
| ['0'-'9']+ '.'? ['0'-'9']* 'e' '-'? ['0'-'9']+ as f
    { NUM (float_of_string f) }
| ['0'-'9']+ '.'? ['0'-'9']* ['m' 'n' 'u' 'p' 'f' 'k' 'M' 'G' 'T'] as f
    { 
        let len = String.length f in
        let m = match f.[len-1] with
        | 'm' -> 1e-3
        | 'u' -> 1e-6
        | 'n' -> 1e-9
        | 'p' -> 1e-12
        | 'f' -> 1e-15
        | 'k' -> 1e3
        | 'M' -> 1e6
        | 'G' -> 1e9
        | 'T' -> 1e12
        | _ -> failwith "Prefix not valid" in
        let num = String.sub f 0 (len - 1) in
        NUM (m *. (float_of_string num))
    }
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
| "//"
    { PAR }
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
    { 
    let pos = Lexing.lexeme_start lexbuf in 
        raise (Error (Printf.sprintf "At offset %d: unexpected character: %c.\n" pos (Lexing.lexeme_char lexbuf pos))) 
    }