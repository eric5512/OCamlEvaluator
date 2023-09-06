let var = ref "";;
let mode = ref "";;
let file = ref "";;

let process (line : string): (Operation.operation_t, string) Either.t =
  let linebuf = Lexing.from_string line in
  try
    Left (Parser.main Lexer.token linebuf)
  with
  | Lexer.Error msg ->
      Right (Format.sprintf "%s\n%!" msg)
  | Parser.Error ->
      Right (Format.sprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf))
  | Not_found ->
      Right "Unknown identifier\n%!";;

let execute op = 
  Printf.printf "%s\n%!"
  (match !mode with
  | "simplify" -> 
    (Operation.simplify op |> Operation.string_of_operation) 
  | "evaluate" -> 
    (Operation.eval Operation.variables Operation.functions op |> string_of_float)
  | "derivate" -> 
    (Operation.derivate !var op |> Operation.simplify |> Operation.string_of_operation)
  | _ as m -> raise (Arg.Bad (m)));;
  
let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      try
        (match process line with
          | Left op -> execute op
          | Right msg -> print_string msg)
      with
      | Function.Apply_error (e, g) ->
        Printf.printf "Error applying arguments to function. %d given and %d expected\n%!" g e;;

let rec repeat channel =
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel;;

let main =
  begin
    let speclist = [
    ("--var", Arg.Set_string var, "Selects the variable when mode is \"derivate\"");
    ("--file", Arg.Set_string file, "Selects the input file");
    ("--mode", Arg.Symbol (["simplify"; "evaluate"; "derivate"], (fun s -> mode := s)), " Selects the mode");
    ]
    in let usage_msg = "oeval --mode <mode> --var <variable_name>"
    in Arg.parse speclist print_endline usage_msg;
    
    let in_chnl = if !file <> "" then open_in !file else stdin in
    repeat (Lexing.from_channel in_chnl);
  end