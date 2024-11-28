open Expression;;

(* TODO: Implement list command to list functions and variables available *)

let list (): string = 
  "Variables:\n" ^
  Hashtbl.fold (fun n v acc -> acc ^ (Printf.sprintf "\t%s = %f\n" n v)) variables "" ^
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