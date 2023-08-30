open Operation;;

let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
    Printf.printf "%f\n%!" (eval variables functions (Parser.main Lexer.token linebuf))
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s\n%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)
  | Not_found ->
      Printf.fprintf stderr "Unknown identifier\n%!";;

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line;;

let rec repeat channel =
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel;;
  
let () =
  repeat (Lexing.from_channel stdin);;