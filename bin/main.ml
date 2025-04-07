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
          | Left (Op op) -> let res = Eval.eval Expression.variables op in 
            Expression.add_var "ans" res;
            res |> string_of_float |> Printf.fprintf stdout "%s\n%!"
          | Left (FunDef (n, args, v)) -> Hashtbl.add Expression.functions n (CFun (args, v))
          | Left (VarDef (n, v)) -> Expression.add_var n (Eval.eval Expression.variables v)
          | Left (Der (var, op)) -> Derivate.derivate var op |> Simplify.simplify |> Expression.string_of_operation |> Printf.fprintf stdout "%s\n%!"
          | Left (Sim ex) -> Simplify.simplify ex |> Expression.string_of_operation |> Printf.fprintf stdout "%s\n%!"
          | Left (Conv (src, dst, value)) -> Convert.convert src dst value |> string_of_float |> Printf.fprintf stdout "%s\n%!"
          | Left (Base (base, num)) -> Base.base_change base num |> Printf.fprintf stdout "%s\n%!"
          | Left (Solve (op, var, init)) -> let res = Solve.find_zero op var (Eval.eval Expression.variables init) 1e-10 in 
          Expression.add_var "ans" res;
            Printf.fprintf stdout "%f\n%!" res
          | Left (Plot (op, var, b, e)) -> Plot.plot op var (Eval.eval Expression.variables b) (Eval.eval Expression.variables e) |> ignore
          | Left (Listc) -> Printf.printf "%s%!\n" (Listc.list ())
          | Left (Help) -> Printf.printf "%s%!\n" (Help.help ())
          | Right msg -> Printf.printf "%s%!\n" msg);
        flush stdout
      with
      | Expression.Apply_error (e, g) ->
        Printf.printf "Error applying arguments to function. %d given and %d expected\n%!\n" g e
      | Expression.Unknown_variable s ->
        Printf.printf "Unknown variable: \"%s\"\n%!\n" s
      | Expression.Unknown_function s ->
          Printf.printf "Unknown function: \"%s\"\n%!\n" s
      | Expression.Incompatible_magnitudes (m1, m2) ->
        Printf.printf "Non existing conversion between \"%s\" and \"%s\"%!\n" m1 m2
      | Expression.Unit_nonimplemented u ->
          Printf.printf "Non existing unit \"%s\"%!\n" u
      | Expression.Missing_derivate f ->
        Printf.printf "Function \"%s\" is missing the derivate%!\n" f;;

let rec repeat channel =
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel;;

let repeat_erepl (): unit =
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

let () =
  begin
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> Repl.disable_raw_mode (); print_endline ""; exit 0)); (* Handle the Ctrl-C signal *)

    let speclist = [
    ("--file", Arg.Set_string file, "Selects the input file to evaluate");
    ("--load", Arg.Set_string load, "Selects a file to load definitions");
    ("--erepl", Arg.Set erepl, "Disable Enchanced repl mode");
    ]
    in let usage_msg = "oeval"
    in Arg.parse speclist print_endline usage_msg;

    erepl := not !erepl;

    if !load <> "" then 
      open_in !load 
      |> Lexing.from_channel 
      |> repeat;
    
    let in_chnl = 
      if !file <> "" then 
        ((if !erepl then erepl := false);
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
