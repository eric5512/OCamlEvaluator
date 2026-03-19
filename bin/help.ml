let help () = 
  Base.help_short ^ "\n" ^
  Convert.help_short ^ "\n" ^
  Derivate.help_short ^ "\n" ^
  Listc.help_short ^ "\n" ^
  Plot.help_short ^ "\n" ^
  Solve.help_short ^ "\n" ^
  Simplify.help_short ^ "\n" ^
  "HELP cmd //Enter the lowercase command name to get extended help";;

let help_cmd (command: string) = match command with
  | "base"  -> Base.help_long
  | "conv"  -> Convert.help_long
  | "der"   -> Derivate.help_long
  | "list"  -> Listc.help_long
  | "plot"  -> Plot.help_long
  | "solve" -> Solve.help_long
  | "sim"   -> Simplify.help_long
  | _       -> raise (Failure "Not implemented")

