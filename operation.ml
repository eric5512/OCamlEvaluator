type operation =  Add of operation * operation
                | Sub of operation * operation
                | Mul of operation * operation
                | Div of operation * operation
                | Pow of operation * operation
                | Neg of operation
                | Fun of string * operation
                | Var of string
                | Val of float;;

let rec eval var_env fun_env = function
  | Add (l, r) -> (eval var_env fun_env l) +. (eval var_env fun_env r)
  | Sub (l, r) -> (eval var_env fun_env l) -. (eval var_env fun_env r)
  | Mul (l, r) -> (eval var_env fun_env l) *. (eval var_env fun_env r)
  | Div (l, r) -> (eval var_env fun_env l) /. (eval var_env fun_env r)
  | Pow (l, r) -> Float.pow (eval var_env fun_env l) (eval var_env fun_env r)
  | Neg o -> Float.neg (eval var_env fun_env o)
  | Fun (f, o) -> Hashtbl.find fun_env f (eval var_env fun_env o)
  | Var s -> Hashtbl.find var_env s
  | Val v -> v;;

let rec partial_eval var_env fun_env = function
  | Add (l, r) -> Add ((partial_eval var_env fun_env l), (partial_eval var_env fun_env r))
  | Sub (l, r) -> Sub ((partial_eval var_env fun_env l), (partial_eval var_env fun_env r))
  | Mul (l, r) -> Mul ((partial_eval var_env fun_env l), (partial_eval var_env fun_env r))
  | Div (l, r) -> Div ((partial_eval var_env fun_env l), (partial_eval var_env fun_env r))
  | Pow (l, r) -> Pow ((partial_eval var_env fun_env l), (partial_eval var_env fun_env r))
  | Neg o -> Neg (partial_eval var_env fun_env o)
  | Fun (f, o) -> Hashtbl.find fun_env f (partial_eval var_env fun_env o)
  | Var s -> (match Hashtbl.find_opt var_env s with
    | None -> Var s
    | Some v -> Val v)
  | Val v -> Val v;;

let simplify expr = 
  let rec aux = function
      | Add (l, r) -> (match (l, r) with
        | (Val l, Val r) -> (Val (l +. r), true)
        | (_, Val 0.0) -> (l, true)
        | (Val 0.0, _) -> (r, true)
        | (Neg l, r) when l = r -> (Val 0.0, true)
        | (l, Neg r) when l = r -> (Val 0.0, true)
        | _ -> let (l, c1) = aux l in let (r, c2) = aux r in (Add (l, r), c1 || c2))
      | Sub (l, r) -> (match (l, r) with
        | (Val l, Val r) -> (Val (l -. r), true)
        | (_, Val 0.0) -> (l, true)
        | (Val 0.0, _) -> (Neg r, true)
        | (_, Neg r) -> (Add (l, r), true)
        | _ -> if l = r then (Val 0.0, false) else let (l, c1) = aux l in let (r, c2) = aux r in (Sub (l, r), c1 || c2))
      | Mul (l, r) -> (match (l, r) with
        | (Val l, Val r) -> (Val (l *. r), true)
        | (_, Val 1.0) -> (l, true)
        | (Val 1.0, _) -> (r, true)
        | (_, Val 0.0) -> (Val 0.0, true)
        | (Val 0.0, _) -> (Val 0.0, true)
        | _ -> let (l, c1) = aux l in let (r, c2) = aux r in (Mul (l, r), c1 || c2))
      | Div (l, r) -> (match (l, r) with
        | (Val l, Val r) -> (Val (l *. r), true)
        | (_, Val 1.0) -> (l, true)
        | (_, Val 0.0) -> (Var "inf", true)
        | (Val 0.0, _) -> (Val 0.0, true)
        | _ -> if l = r then (Val 1.0, true) else let (l, c1) = aux l in let (r, c2) = aux r in (Div (l, r), c1 || c2))
      | Pow (l, r) -> (match (l, r) with
        | (Val l, Val r) -> (Val (l *. r), true)
        | (_, Val 1.0) -> (l, true)
        | (Val 1.0, _) -> (Val 1.0, true)
        | (_, Val 0.0) -> (Val 1.0, true)
        | (Val 0.0, _) -> (Val 0.0, true)
        | _ -> let (l, c1) = aux l in let (r, c2) = aux r in (Pow (l, r), c1 || c2))
      | Neg o -> (match o with
        | Neg no -> (no, true)
        | _ -> (Neg o, false))
      | Fun (f, o) -> let (o, c) = aux o in (Fun (f, o), c)
      | _ as c -> (c, false) in
    let repeat = ref true in
    let op = ref expr in
    while !repeat do
      let (o, r) = aux !op in
        op := o; repeat := r;
    done;
    !op;;


let operation_priority = function
  | Add _ -> 2
  | Sub _ -> 2
  | Mul _ -> 1
  | Div _ -> 1
  | Pow _ -> 0
  | Neg _ -> 3
  | Fun _ -> 0
  | Var _ -> 0
  | Val _ -> 0;;

let rec string_of_operation = 
  let surround_par_if_higher a n = 
    if operation_priority a <= operation_priority n then 
      "(" ^ (string_of_operation n) ^ ")" 
    else 
      string_of_operation n 
    in
      function
        | Add (l, r) as a -> 
          (surround_par_if_higher a l) ^ "+" ^ (surround_par_if_higher a r)
        | Sub (l, r) as a -> 
          (surround_par_if_higher a l) ^ "-" ^ (surround_par_if_higher a r)
        | Mul (l, r) as a -> 
          (surround_par_if_higher a l) ^ "*" ^ (surround_par_if_higher a r)
        | Div (l, r) as a -> 
          (surround_par_if_higher a l) ^ "/" ^ (surround_par_if_higher a r)
        | Pow (l, r) as a -> 
          (surround_par_if_higher a l) ^ "^" ^ (surround_par_if_higher a r)
        | Neg o as a -> "-" ^ (surround_par_if_higher a o)
        | Fun (f, o) as a -> f ^ "(" ^ (surround_par_if_higher a o) ^ ")"
        | Var s -> s
        | Val v -> string_of_float v;;

let rec derivate = function (* TODO: Add support for derivates *)
  | Add (l, r) -> Add(derivate l, derivate r)
  | Sub (l, r) -> Sub(derivate l, derivate r)
  | Mul (l, r) -> Mul(Mul (derivate l, r), Mul (l, derivate r))
  | Div (l, r) -> Div(Sub (Mul (derivate l, r), Mul (l, derivate r)), Pow (r, Val 2.0))
  | Pow (l, r) -> Pow(derivate l, derivate r)
  | Neg o -> Neg (derivate o)
  | Fun (f, o) -> (derivate o)
  | Var s -> Val 1.0
  | Val v -> Val 0.0;;



let functions: (string, float -> float) Hashtbl.t = Hashtbl.create 10;;

let function_list: (string * (float -> float)) list = ["sin", Float.sin;
                                                       "cos", Float.cos;
                                                       "tan", Float.tan;
                                                       "asin", Float.asin;
                                                       "acos", Float.acos;
                                                       "atan", Float.atan;];;

let variables: (string, float) Hashtbl.t = Hashtbl.create 10;;

let variable_list: (string * float) list = ["pi", Float.pi;
                                            "inf", Float.infinity;
                                            "euler", 2.718281828459045;];;

let () = Hashtbl.add_seq functions (List.to_seq function_list);
         Hashtbl.add_seq variables (List.to_seq variable_list);;