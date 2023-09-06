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

let rec eval (var_env: (string, float) Hashtbl.t) (fun_env: (string, Function.function_t) Hashtbl.t) = function
  | Bop (op, l, r) -> bop_to_op op (eval var_env fun_env l) (eval var_env fun_env r)
  | Neg o -> Float.neg (eval var_env fun_env o)
  | Fun (f, o) -> Function.apply_func (Hashtbl.find fun_env f) (Array.map (eval var_env fun_env) o)
  | Var s -> Hashtbl.find var_env s
  | Val v -> v;;

let rec partial_eval var_env fun_env = function
  | Bop (op, l, r) -> Bop (op, (partial_eval var_env fun_env l), (partial_eval var_env fun_env r))
  | Neg o -> Neg (partial_eval var_env fun_env o)
  | Fun (f, o) -> Hashtbl.find fun_env f (Array.map (partial_eval var_env fun_env) o)
  | Var s -> (match Hashtbl.find_opt var_env s with
    | None -> Var s
    | Some v -> Val v)
  | Val v -> Val v;;



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
      
      | Fun (f, o) -> let acc = Array.fold_left (fun (x1, y1) (x2, y2) -> ( x2::x1, y1 || y2)) ([], false) (Array.map aux o) in (Fun (f, fst acc |> Array.of_list), snd acc)
      
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

let rec derivate var = function (* TODO: Add support for derivates *)
  | Bop (Add, l, r) -> Bop (Add, derivate var l, derivate var r)
  | Bop (Sub, l, r) -> Bop (Sub, derivate var l, derivate var r)
  | Bop (Mul, l, r) -> Bop (Mul, Bop (Mul, derivate var l, r), Bop (Mul, l, derivate var r))
  | Bop (Div, l, r) -> Bop (Div, Bop (Sub, Bop (Mul, derivate var l, r), Bop (Mul, l, derivate var r)), Bop (Pow, r, Val 2.0))
  | Bop (Pow, l, r) -> Bop (Pow, derivate var l, derivate var r)
  | Neg o -> Neg (derivate var o)
  | Fun (f, o) -> Fun (f,o)
  | Var s -> if s = var then Val 1.0 else Var s
  | Val v -> Val 0.0;;



let functions: (string, Function.function_t) Hashtbl.t = Hashtbl.create 10;;

let function_list: (string * Function.function_t) list = [
  "sin", Function.Arg1 Float.sin;
  "cos", Function.Arg1 Float.cos;
  "tan", Function.Arg1 Float.tan;
  "asin", Function.Arg1 Float.asin;
  "acos", Function.Arg1 Float.acos;
  "atan", Function.Arg1 Float.atan;
  "log10", Function.Arg1 Float.log10;
  "log2", Function.Arg1 Float.log2;
  "ln", Function.Arg1 Float.log;
  ];;

let variables: (string, float) Hashtbl.t = Hashtbl.create 10;;

let variable_list: (string * float) list = ["pi", Float.pi;
                                            "inf", Float.infinity;
                                            "euler", 2.718281828459045;];;

let () = Hashtbl.add_seq functions (List.to_seq function_list);
         Hashtbl.add_seq variables (List.to_seq variable_list);;