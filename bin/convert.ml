open Expression;;

let convert (src: string) (dst: string) (operation: operation_t): float = let value = Eval.eval variables operation in
match (String.lowercase_ascii src, String.lowercase_ascii dst) with
  | ("dbm", "v") -> (sqrt (50./.1000.)) *. (Float.pow 10. (value/.20.))
  | ("v", "dbm") -> 10. *. Float.log10 ((Float.pow value 2.) *. 1000. /. 50.)
  | ("m", "feet") -> 3.28 *. value
  | ("feet", "m") -> value /. 3.28
  | ("l", "gallon") -> 0.264 *. value
  | ("gallon", "l") -> value /. 0.264
  | ("degc", "degf") -> value *. 9. /. 5. +. 32.
  | ("degf", "degc") -> (value -. 32.) *. 5./.9.
  | ("kg", "pound") -> 2.2046 *. value
  | ("pound", "kg") -> value /. 2.2046
  | ("km", "mile") -> 0.62 *. value
  | ("mile", "km") -> value /. 0.62
  | ("cm", "inch") -> 0.39 *. value
  | ("inch", "cm") -> value /. 0.39
  | _ as pair -> raise (Conversion_nonimplemented pair);;