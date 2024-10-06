open Expression;;

let rec eval var_env =
  let apply_func (f: function_t) (a: arg_list_t): float = let len = List.length a in 
  let zip a b = 
    let len_a = List.length a in 
    let rec aux a b acc = match a,b with
      | ([], []) -> acc
      | (_,[]) -> raise (Apply_error (len,len_a))
      | ([],_) -> raise (Apply_error (len,len_a))
      | (x::xs, y::ys) -> aux xs ys ((x,y)::acc) in
    aux a b [] in
  match f, a with
    | (OFun (Arg1 f), [x1]) -> f x1
    | (OFun (Arg2 f), [x1; x2]) -> f x1 x2
    | (OFun (Arg3 f), [x1; x2; x3]) -> f x1 x2 x3
    | (OFun (Arg4 f), [x1; x2; x3; x4]) -> f x1 x2 x3 x4
    | (OFun (Arg5 f), [x1; x2; x3; x4; x5]) -> f x1 x2 x3 x4 x5
    | (OFun (Arg6 f), [x1; x2; x3; x4; x5; x6]) -> f x1 x2 x3 x4 x5 x6
    | (OFun (Arg7 f), [x1; x2; x3; x4; x5; x6; x7]) -> f x1 x2 x3 x4 x5 x6 x7
    | (OFun (Arg8 f), [x1; x2; x3; x4; x5; x6; x7; x8]) -> f x1 x2 x3 x4 x5 x6 x7 x8
    | (CFun (args, op), _) -> eval (Hashtbl.of_seq (zip args a |> List.to_seq)) op
    | (OFun f, _) -> raise (Apply_error (ocaml_func_number f, len)) in function
  | Bop (op, l, r) -> bop_to_op op (eval var_env l) (eval var_env r)
  | Neg o -> Float.neg (eval var_env o)
  | Fun (f, o) -> (try apply_func (Hashtbl.find functions f) (List.map (eval var_env) (Array.to_list o)) with Not_found -> raise (Unknown_function f))
  | Var s -> (try Hashtbl.find var_env s with Not_found -> raise (Unknown_variable s))  
  | Val v -> v;;
