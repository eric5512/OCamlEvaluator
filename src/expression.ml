type bop_t = Add | Sub | Mul | Div | Pow

let bop_to_op = function 
  | Add -> (Float.add) | Sub -> (Float.sub)
  | Mul -> (Float.mul) | Div -> (Float.div)
  | Pow -> (Float.pow);;

let bop_to_str = function
  | Add -> "+" | Sub -> "-"
  | Mul -> "*" | Div -> "/"
  | Pow -> "^";;

let bop_priority = function
  | Add -> 3 | Sub -> 3
  | Mul -> 2 | Div -> 2
  | Pow -> 1;;

type operation_t =  Bop of bop_t * operation_t * operation_t
                  | Neg of operation_t
                  | Fun of string * (operation_t array)
                  | Var of string
                  | Val of float;;

exception Apply_error of int * int

type arg_list_t = float list

type ocaml_function_t = Arg1 of (float -> float)
  | Arg2 of (float -> float -> float)
  | Arg3 of (float -> float -> float -> float)
  | Arg4 of (float -> float -> float -> float -> float)
  | Arg5 of (float -> float -> float -> float -> float -> float)
  | Arg6 of (float -> float -> float -> float -> float -> float -> float)
  | Arg7 of (float -> float -> float -> float -> float -> float -> float -> float)
  | Arg8 of (float -> float -> float -> float -> float -> float -> float -> float -> float);;

type function_t = OFun of ocaml_function_t
  | CFun of (string list * operation_t)

let ocaml_func_number = function
  | Arg1 _ -> 1 | Arg2 _ -> 2
  | Arg3 _ -> 3 | Arg4 _ -> 4
  | Arg5 _ -> 5 | Arg6 _ -> 6
  | Arg7 _ -> 7 | Arg8 _ -> 8;;

let functions: (string, function_t) Hashtbl.t = Hashtbl.create 10;;

let variables: (string, float) Hashtbl.t = Hashtbl.create 10;;

exception Unknown_variable of string;;

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
  | Fun (f, o) -> apply_func (Hashtbl.find functions f) (List.map (eval var_env) (Array.to_list o))
  | Var s -> (try Hashtbl.find var_env s with Not_found -> raise (Unknown_variable s))  
  | Val v -> v;;

let rec partial_eval = function
  | Bop (op, l, r) -> Bop (op, (partial_eval l), (partial_eval r))
  | Neg o -> Neg (partial_eval o)
  | Fun (f, o) -> failwith "Not implemented" (*apply_func (Hashtbl.find functions f) (Array.map (partial_eval) o*)
  | Var s -> (match Hashtbl.find_opt variables s with
    | None -> Var s
    | Some v -> Val v)
  | Val v -> Val v;;

exception Conversion_nonimplemented of (string * string);;

let convert (src: string) (dst: string) (value: float): float = match (src, dst) with
  | ("dbm", "v") -> (sqrt (50./.1000.)) *. (Float.pow 10. (value/.20.))
  | _ as pair -> raise (Conversion_nonimplemented pair);;

let simplify expr = 
  let rec aux = function
      | Bop (op, Val l, Val r) -> (Val ((bop_to_op op) l r), true)

      (* generic add *)
      | Bop (Add, l, Val 0.0) -> (l, true)
      | Bop (Add, Val 0.0, r) -> (r, true)
      | Bop (Add, Neg l, r) when l = r -> (Val 0.0, true)
      | Bop (Add, l, Neg r) when l = r -> (Val 0.0, true)
      
      (* add to mul rules *)
      | Bop (Add, Var l, Var r) when l = r -> (Bop (Mul, Val 2.0, Var l), true)
      | Bop (Add, Var l, Bop (Mul, Val v, Var r)) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Add, Var l, Bop (Mul, Var r, Val v)) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Add, Bop (Mul, Var l, Val v), Var r) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Add, Bop (Mul, Val v, Var l), Var r) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Add, Bop (Mul, Val vl, Var l), Bop (Mul, Val vr, Var r)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Add, Bop (Mul, Var l, Val vl), Bop (Mul, Val vr, Var r)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Add, Bop (Mul, Var l, Val vl), Bop (Mul, Var r, Val vr)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Add, Bop (Mul, Val vl, Var l), Bop (Mul, Var r, Val vr)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)

      (* generic sub *)
      | Bop (Sub, l, Val 0.0) -> (l, true)
      | Bop (Sub, Val 0.0, r) -> (Neg r, true)
      | Bop (Sub, l, Neg r) -> (Bop (Add, l, r), true)
      | Bop (Sub, Neg l, r) -> (Neg (Bop (Add, l, r)), true)
      | Bop (Sub, l, r) when l = r -> (Val 0.0, true)
      
      (* sub to mul rules *)
      | Bop (Sub, Var l, Bop (Mul, Val v, Var r)) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Sub, Var l, Bop (Mul, Var r, Val v)) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Sub, Bop (Mul, Var l, Val v), Var r) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Sub, Bop (Mul, Val v, Var l), Var r) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Sub, Bop (Mul, Val vl, Var l), Bop (Mul, Val vr, Var r)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Sub, Bop (Mul, Var l, Val vl), Bop (Mul, Val vr, Var r)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Sub, Bop (Mul, Var l, Val vl), Bop (Mul, Var r, Val vr)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Sub, Bop (Mul, Val vl, Var l), Bop (Mul, Var r, Val vr)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)

      (* generic mul *)
      | Bop (Mul, l, Val 1.0) -> (l, true)
      | Bop (Mul, Val 1.0, r) -> (r, true)
      | Bop (Mul, _, Val 0.0) -> (Val 0.0, true)
      | Bop (Mul, Val 0.0, _) -> (Val 0.0, true)
      
      (* mul to pow rules *)
      | Bop (Mul, Var l, Var r) when l = r -> (Bop (Pow, Var l, Val 2.0), true)
      | Bop (Mul, Bop (Pow, Var l, Val v), Var r) when l = r -> (Bop (Pow, Var l, Val (v +. 1.0)), true)
      | Bop (Mul, Var l, Bop (Pow, Var r, Val v)) when l = r -> (Bop (Pow, Var l, Val (v +. 1.0)), true)
      | Bop (Mul, Bop (Pow, Var l, Val v), Var r) when l = r -> (Bop (Pow, Var l, Val (v +. 1.0)), true)
      | Bop (Mul, Bop (Pow, Var l, Val vl), Bop (Pow, Var r, Val vr)) when l = r -> (Bop (Pow, Var l, Val (vl +. vr)), true)

      (* generic div *)
      | Bop (Div, l, Val 1.0) -> (l, true)
      | Bop (Div, _, Val 0.0) -> (Val infinity, true)
      | Bop (Div, Val 0.0, _) -> (Val 0.0, true)
      | Bop (Div, l, r) when l = r -> (Val 1.0, true)
      
      (* div to pow rules *)
      | Bop (Div, Var l, Bop (Pow, Val v, Var r)) when l = r -> (Bop (Pow, Val (v +. 1.0), Var l), true)
      | Bop (Div, Var l, Bop (Pow, Var r, Val v)) when l = r -> (Bop (Pow, Val (v +. 1.0), Var l), true)
      | Bop (Div, Bop (Pow, Var l, Val v), Var r) when l = r -> (Bop (Pow, Val (v +. 1.0), Var l), true)
      | Bop (Div, Bop (Pow, Var l, Val vl), Bop (Pow, Var r, Val vr)) when l = r -> (Bop (Pow, Val (vl +. vr), Var l), true)
      
      (* generic pow *)
      | Bop (Pow, l, Val 1.0) -> (l, true)
      | Bop (Pow, Val 1.0, _) -> (Val 1.0, true)
      | Bop (Pow, _, Val 0.0) -> (Val 1.0, true)
      | Bop (Pow, Val 0.0, _) -> (Val 0.0, true)

      | Bop (op, l, r) -> let (l, c1) = aux l in let (r, c2) = aux r in (Bop (op, l, r), c1 || c2)
      
      | Neg o -> (match o with
        | Neg no -> (no, true)
        | _ -> let (o, c) = aux o in (Neg o, c))
      
      | Fun (f, o) -> 
        let acc = 
          Array.fold_left 
            (fun (x1, y1) (x2, y2) -> ( x2::x1, y1 || y2))
            ([], false)
            (Array.map aux o) in 
        (Fun (f, fst acc |> Array.of_list), snd acc)
      
      | _ as c -> (c, false) in
    let repeat = ref true in
    let op = ref expr in
    while !repeat do
      let (o, r) = aux !op in
        op := o; repeat := r;
    done;
    !op;;

let operation_priority = function
  | Bop (op, _, _) -> bop_priority op
  | Neg _ -> 3
  | Fun _ -> 0
  | Var _ -> 0
  | Val _ -> 0;;

let rec string_of_operation = 
  let rec surround_par_if_higher a n = 
    if operation_priority a <= operation_priority n then 
      "(" ^ (string_of_operation n) ^ ")" 
    else 
      string_of_operation n 
    in
      function
        | Bop (op, l, r) as a -> 
          (surround_par_if_higher a l) ^ (bop_to_str op) ^ (surround_par_if_higher a r)
        | Neg o as a -> "-" ^ (surround_par_if_higher a o)
        | Fun (f, o) -> f ^ "(" ^ (String.concat "," (Array.map string_of_operation o |> Array.to_list)) ^ ")"
        | Var s -> s
        | Val v -> string_of_float v;;

let op_eq (op1: operation_t) (op2: operation_t): bool = 
  let rec zip l1 l2 = 
    let rec aux l1 l2 acc = 
    match l1,l2 with
      | ([], []) -> List.rev acc
      | (x::xs, y::ys) -> aux xs ys (aux xs ys ((x,y)::acc))
      | _ -> failwith "Unreacheable op_eq" in
    aux l1 l2 [] in
  let rec aux op1 op2 = 
    match op1, op2 with
    | (Bop (op1, a1, b1), Bop (op2, a2, b2)) when op1 = op2 && op1 <> Sub && op1 <> Pow && op1 <> Div ->
      (aux a1 b2) && (aux a2 b1)
    | (Fun (a1,b1), Fun (a2,b2)) ->
      (a1 = a2) && (Array.length b1 = Array.length b2) && (List.fold_left (fun x (y1, y2) -> x && (aux y1 y2)) true (zip (Array.to_list b1) (Array.to_list b2)))
    | (a,b) when a = b -> 
      true
    | _ -> 
      false in
  aux op1 op2;;

exception Missing_derivate of string;;

let rec derivate var = function (* TODO: Add support for logarithmic derivates *)
  | Bop (Add, l, r) -> Bop (Add, derivate var l, derivate var r)
  | Bop (Sub, l, r) -> Bop (Sub, derivate var l, derivate var r)
  | Bop (Mul, l, r) -> Bop (Mul, Bop (Mul, derivate var l, r), Bop (Mul, l, derivate var r))
  | Bop (Div, l, r) -> Bop (Div, Bop (Sub, Bop (Mul, derivate var l, r), Bop (Mul, l, derivate var r)), Bop (Pow, r, Val 2.0))
  | Bop (Pow, Var s, Val v) when s = var -> Bop (Mul, Val v, Bop (Pow, Var s, Val (v-.1.0)))
  | Bop (Pow, l, r) -> Bop (Pow, derivate var l, derivate var r)
  | Neg o -> Neg (derivate var o)
  | Fun (f, o) -> Bop (Mul, (match f, o with
    | ("sin", o) -> Fun ("cos", o)
    | ("asin", o) -> Bop (Div, Val 1.0, Bop (Pow, Bop (Sub, Val 1.0, Bop (Pow, Var var, Val 2.0)), Val 0.5))
    | ("cos", o) -> Neg (Fun ("sin", o))
    | ("acos", o) -> Bop (Div, Neg (Val 1.0), Bop (Pow, Bop (Sub, Val 1.0, Bop (Pow, Var var, Val 2.0)), Val 0.5))
    | ("tan", o) -> Bop (Div, Val 1.0, Bop (Pow, Fun ("cos", o), Val 2.0))
    | ("ln", o) -> Bop (Div, Val 1.0, o.(0))
    | ("log10", o) -> Bop (Div, Fun ("log10", [| Var "euler" |]), o.(0))
    | ("log2", o) -> Bop (Div, Fun ("log2", [| Var "euler" |]), o.(0))
    | _ -> raise (Missing_derivate f)), derivate var o.(0))
  | Var s -> if s = var then Val 1.0 else Var s
  | Val v -> Val 0.0;;

type expr_t = Op of operation_t
| FunDef of (string * (string list) * operation_t)
| VarDef of (string * operation_t)
| Der of (string * operation_t)
| Sim of operation_t
| Conv of (string * string * float)

let variable_list: (string * float) list = [
  "pi", Float.pi;
  "inf", Float.infinity;
  "euler", 2.718281828459045;
  "epsilon", Float.epsilon;
];;
  
let function_list: (string * function_t) list = [
  "sin", OFun (Arg1 Float.sin);
  "cos", OFun (Arg1 Float.cos);
  "tan", OFun (Arg1 Float.tan);
  "asin", OFun (Arg1 Float.asin);
  "acos", OFun (Arg1 Float.acos);
  "atan", OFun (Arg1 Float.atan);

  "sinh", OFun (Arg1 Float.sinh);
  "cosh", OFun (Arg1 Float.cosh);
  "tanh", OFun (Arg1 Float.tanh);
  "asinh", OFun (Arg1 Float.asinh);
  "acosh", OFun (Arg1 Float.acosh);
  "atanh", OFun (Arg1 Float.atanh);
  
  "log10", OFun (Arg1 Float.log10);
  "log2", OFun (Arg1 Float.log2);
  "ln", OFun (Arg1 Float.log);

  "mod", OFun (Arg2 Float.rem);
  "sqrt", OFun (Arg1 Float.sqrt);
  "abs", OFun (Arg1 Float.abs);
];;

let () = Hashtbl.add_seq functions (List.to_seq function_list);
         Hashtbl.add_seq variables (List.to_seq variable_list);;