let var = ref "";;
let mode = ref "";;
let file = ref "";;
let load = ref "";;
let erepl = ref false;;

let process (line : string): (Expression.expr_t, string) Either.t =
  let linebuf = Lexing.from_string line in
  try
    Left (Parser.main Lexer.token linebuf)
  with
  | Lexer.Error msg ->
    Right msg
  | Parser.Error ->
    Right (Format.sprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf))
  | Not_found ->
    Right "Unknown identifier";;

let execute_op op = 
  Printf.printf "%s\n%!"
  (match !mode with
  | "simplify" -> 
    (Expression.simplify op |> Expression.string_of_operation)
  | "evaluate" -> 
    (Expression.eval Expression.variables op |> string_of_float)
  | "derivate" -> 
    (Expression.derivate !var op |> Expression.simplify |> Expression.string_of_operation)
  | _ as m -> raise (Arg.Bad (m)));;

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      try
        (match process line with
          | Left (Op op) -> execute_op op
          | Left (FunDef (n, args, v)) -> Hashtbl.add Expression.functions n (CFun (args, v))
          | Left (VarDef (n, v)) -> Hashtbl.add Expression.variables n (Expression.eval Expression.variables v)
          | Right msg -> Printf.printf "%s%!\n" msg)
      with
      | Expression.Apply_error (g, e) ->
        Printf.printf "Error applying arguments to function. %d given and %d expected\n%!" g e
      | Expression.Unknown_variable s ->
        Printf.printf "Unknown variable: \"%s\"\n%!" s;;

let rec repeat channel =
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel;;

let main =
  begin
    let speclist = [
    ("--var", Arg.Set_string var, "Selects the variable when mode is \"derivate\"");
    ("--file", Arg.Set_string file, "Selects the input file to evaluate");
    ("--mode", Arg.Symbol (["simplify"; "evaluate"; "derivate"], (fun s -> mode := s)), " Selects the mode");
    ("--load", Arg.Set_string load, "Selects a file to load definitions");
    ("--erepl", Arg.Set erepl, "Selects a file to load definitions");
    ]
    in let usage_msg = "oeval --mode <mode>"
    in Arg.parse speclist print_endline usage_msg;

    if !load <> "" then 
      open_in !load 
      |> Lexing.from_channel 
      |> repeat;
    
    let in_chnl = 
      if !file <> "" then 
        (if !erepl then
          (print_endline "Warning: The enchanced repl mode is only available for stdin";
          stdin)
        else
          open_in !file) 
      else 
        stdin in
      repeat (Lexing.from_channel in_chnl);
  end