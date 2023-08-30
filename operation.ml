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

let rec derivate = function (* TODO: Add support for derivates *)
                | Add (l, r) -> Add(derivate l, derivate r)
                | Sub (l, r) -> Sub(derivate l, derivate r)
                | Mul (l, r) -> Mul(derivate l, derivate r)
                | Div (l, r) -> Div(derivate l, derivate r)
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