open Expression;;

let list (): string = 
  "Variables:\n" ^
  Hashtbl.fold (fun n v acc -> acc ^ (Printf.sprintf "\t%s = %.10e\n" n v)) variables "" ^
  "\nFunctions:\n" ^
  Hashtbl.fold (
    fun n f acc -> 
      acc ^ (Printf.sprintf "\t%s" n) ^ (
        match f with
        | OFun f -> (match f with
          | Arg1 _ -> "(x)"
          | Arg2 _ -> "(x, y)"
          | Arg3 _ -> "(x, y, z)"
          | Arg4 _ -> "(x, y, z, u)"
          | Arg5 _ -> "(x, y, z, u, v)"
          | Arg6 _ -> "(x, y, z, u, v, w)")
        | CFun (sl, op) -> 
          "(" ^ 
          (String.concat ", " sl) ^ 
          ") := " ^ 
          string_of_operation op
      ) ^ "\n"
    ) functions "";;