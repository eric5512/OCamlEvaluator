type operation =  Add of operation * operation
                | Sub of operation * operation
                | Mul of operation * operation
                | Div of operation * operation
                | Pow of operation * operation
                | Fun of operation list * (float list -> float)
                | Var of string
                | Val of float;;

let rec eval env = function
                | Add (l, r) -> (eval env l) +. (eval env r)
                | Sub (l, r) -> (eval env l) -. (eval env r)
                | Mul (l, r) -> (eval env l) *. (eval env r)
                | Div (l, r) -> (eval env l) /. (eval env r)
                | Pow (l, r) -> Float.pow (eval env l) (eval env r)
                | Fun (l, f) -> f (List.map (eval env) l)
                | Var s -> Hashtbl.find env s
                | Val v -> v;;

let rec parse str = 

let () = print_string "Hello world\n";;