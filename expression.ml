type operation =  Add of operation * operation
                | Sub of operation * operation
                | Mul of operation * operation
                | Div of operation * operation
                | Pow of operation * operation
                | Var of string
                | Val of float;;

let split_on_char str chr = let i = String.index str chr in 
                        (String.sub str 0 i, String.sub str (i+1) ((String.length str)-i-1));;

let rec eval env = function
                | Add (l, r) -> (eval env l) +. (eval env r)
                | Sub (l, r) -> (eval env l) -. (eval env r)
                | Mul (l, r) -> (eval env l) *. (eval env r)
                | Div (l, r) -> (eval env l) /. (eval env r)
                | Pow (l, r) -> Float.pow (eval env l) (eval env r)
                | Var s -> Hashtbl.find env s
                | Val v -> v;;


let rec parse str =      if String.contains str '-' then let (l, r) = split_on_char str '-' in Sub (parse l, parse r)
                    else if String.contains str '+' then let (l, r) = split_on_char str '+' in Add (parse l, parse r)
                    else if String.contains str '*' then let (l, r) = split_on_char str '*' in Mul (parse l, parse r)
                    else if String.contains str '/' then let (l, r) = split_on_char str '/' in Div (parse l, parse r)
                    else if String.contains str '^' then let (l, r) = split_on_char str '^' in Pow (parse l, parse r)
                    else let f = Float.of_string_opt str in if f = None then Var str
                    else Val (Float.of_string str);;

