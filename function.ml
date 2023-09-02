exception Apply_error of int * int

type arg_list_t = float array

type function_t = Arg1 of (float -> float)
  | Arg2 of (float -> float -> float)
  | Arg3 of (float -> float -> float -> float)
  | Arg4 of (float -> float -> float -> float -> float)
  | Arg5 of (float -> float -> float -> float -> float -> float)
  | Arg6 of (float -> float -> float -> float -> float -> float -> float)
  | Arg7 of (float -> float -> float -> float -> float -> float -> float -> float)
  | Arg8 of (float -> float -> float -> float -> float -> float -> float -> float -> float);;

let func_number = function
  | Arg1 _ -> 1 | Arg2 _ -> 2
  | Arg3 _ -> 3 | Arg4 _ -> 4
  | Arg5 _ -> 5 | Arg6 _ -> 6
  | Arg7 _ -> 7 | Arg8 _ -> 8;;

let apply_func (f: function_t) (a: arg_list_t): float = let len = Array.length a in 
match f with
  | Arg1 f when len = 1 -> f a.(0)
  | Arg2 f when len = 2 -> f a.(0) a.(1)
  | Arg3 f when len = 3 -> f a.(0) a.(1) a.(2)
  | Arg4 f when len = 4 -> f a.(0) a.(1) a.(2) a.(3)
  | Arg5 f when len = 5 -> f a.(0) a.(1) a.(2) a.(3) a.(4)
  | Arg6 f when len = 6 -> f a.(0) a.(1) a.(2) a.(3) a.(4) a.(5)
  | Arg7 f when len = 7 -> f a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6)
  | Arg8 f when len = 8 -> f a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6) a.(7)
  | _ -> raise (Apply_error (func_number f, len));;
