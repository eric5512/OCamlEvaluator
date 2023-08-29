(* type operation =  Add of operation * operation
                | Sub of operation * operation
                | Mul of operation * operation
                | Div of operation * operation
                | Pow of operation * operation
                | Fun of operation list * (float list -> float)
                | Var of string
                | Val of float;;

let rec eval env = function
                | Add (l, r) -> (eval env l) +. (eval env r)
                | Sub (l, r) -> (eval env l) -. (eval env r)
                | Mul (l, r) -> (eval env l) *. (eval env r)
                | Div (l, r) -> (eval env l) /. (eval env r)
                | Pow (l, r) -> Float.pow (eval env l) (eval env r)
                | Fun (l, f) -> f (List.map (eval env) l)
                | Var s -> Hashtbl.find env s
                | Val v -> v;; *)

let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
    Printf.printf "%f\n%!" (Parser.main Lexer.token linebuf)
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat channel =
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel
  
let () =
  repeat (Lexing.from_channel stdin)