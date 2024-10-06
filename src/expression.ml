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

exception Apply_error of int * int;;
exception Unknown_variable of string;;
exception Unknown_function of string;;
exception Conversion_nonimplemented of (string * string);;
exception Base_error of string;;
exception Missing_derivate of string;;

type arg_list_t = float list;;

type ocaml_function_t = Arg1 of (float -> float)
  | Arg2 of (float -> float -> float)
  | Arg3 of (float -> float -> float -> float)
  | Arg4 of (float -> float -> float -> float -> float)
  | Arg5 of (float -> float -> float -> float -> float -> float)
  | Arg6 of (float -> float -> float -> float -> float -> float -> float)
  | Arg7 of (float -> float -> float -> float -> float -> float -> float -> float)
  | Arg8 of (float -> float -> float -> float -> float -> float -> float -> float -> float);;

type function_t = OFun of ocaml_function_t
  | CFun of (string list * operation_t);;

let ocaml_func_number = function
  | Arg1 _ -> 1 | Arg2 _ -> 2
  | Arg3 _ -> 3 | Arg4 _ -> 4
  | Arg5 _ -> 5 | Arg6 _ -> 6
  | Arg7 _ -> 7 | Arg8 _ -> 8;;

let functions: (string, function_t) Hashtbl.t = Hashtbl.create 10;;

let variables: (string, float) Hashtbl.t = Hashtbl.create 10;;

let operation_priority = function
  | Bop (op, _, _) -> bop_priority op
  | Neg _ -> 3
  | Fun _ -> 0
  | Var _ -> 0
  | Val _ -> 0;;

let rec operation_contains (target: operation_t) (op: operation_t): bool = if op = target then true else match op with
  | Bop (_, lop, rop) -> operation_contains target lop || operation_contains target rop
  | Neg op -> operation_contains target op
  | Fun (_, ops) -> Array.map (operation_contains target) ops |> Array.fold_left (||) false
  | Var _ -> false
  | Val _ -> false;;

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


type expr_t = Op of operation_t
| FunDef of (string * (string list) * operation_t)
| VarDef of (string * operation_t)
| Der of (string * operation_t)
| Sim of operation_t
| Conv of (string * string * operation_t)
| Base of (string * float)
| Solve of (operation_t * string * operation_t)
| Plot of (operation_t * string * operation_t * operation_t);;

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