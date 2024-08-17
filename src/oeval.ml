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
    Right "Unknown identifier\n"
  | e ->
    Right (Printexc.to_string e^"\n");;

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      try
        (match process line with
          | Left (Op op) -> Expression.eval Expression.variables op |> string_of_float |> Printf.fprintf stdout "%s\n%!"
          | Left (FunDef (n, args, v)) -> Hashtbl.add Expression.functions n (CFun (args, v))
          | Left (VarDef (n, v)) -> Hashtbl.add Expression.variables n (Expression.eval Expression.variables v)
          | Left (Der (var, op)) -> Expression.derivate var op |> Expression.simplify |> Expression.string_of_operation |> Printf.fprintf stdout "%s\n%!"
          | Left (Sim ex) -> Expression.simplify ex |> Expression.string_of_operation |> Printf.fprintf stdout "%s\n%!"
          | Left (Conv (src, dst, value)) -> Expression.convert src dst value |> string_of_float |> Printf.fprintf stdout "%s\n%!"
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

let rec repeat_erepl (): unit =
  try
    (let (read_pipe, write_pipe) = Unix.pipe () in
    Unix.set_close_on_exec read_pipe;
    Unix.set_close_on_exec write_pipe;
    let read_channel = Unix.in_channel_of_descr read_pipe in
    let write_channel = Unix.out_channel_of_descr write_pipe in
    let rec repeat () = 
      let cont = ref true in
      let lexbuff = Lexing.from_channel read_channel in
      try
      while !cont do
        (let str = Repl.read_line () ^ "\n" in
        (output_string write_channel str);
        flush write_channel;
        let optional_line, continue = Lexer.line lexbuff in
        cont := continue;
        process optional_line)
      done
      with
        | End_of_file -> repeat ()
        | Sys_error msg -> Printf.eprintf "Error: %s\n" msg in 
    repeat ();)
  with
  | Unix.Unix_error (err, _, _) -> Printf.eprintf "Unix error: %s\n" (Unix.error_message err)

let main =
  begin
    let speclist = [
    ("--file", Arg.Set_string file, "Selects the input file to evaluate");
    ("--load", Arg.Set_string load, "Selects a file to load definitions");
    ("--erepl", Arg.Set erepl, "Enchanced repl mode");
    ]
    in let usage_msg = "oeval --mode <mode>"
    in Arg.parse speclist print_endline usage_msg;

    if !load <> "" then 
      open_in !load 
      |> Lexing.from_channel 
      |> repeat;
    
    let in_chnl = 
      if !file <> "" then 
        ((if !erepl then
          (print_endline "Warning: The enchanced repl mode is only available for stdin");
          erepl := false
        );
        open_in !file) 
      else 
        stdin in
      if not !erepl then
        repeat (Lexing.from_channel in_chnl)
      else
        try
          Repl.enable_raw_mode ();
          repeat_erepl ();
          Repl.disable_raw_mode ();
        with e -> (Repl.disable_raw_mode (); raise e)
  end