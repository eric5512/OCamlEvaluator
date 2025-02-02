open Expression;;

(* TODO: Derivate user defined functions *)
(* TODO: Partial derivates not working *)

let rec derivate var = function
  | Bop (Add, l, r) -> Bop (Add, derivate var l, derivate var r)
  | Bop (Sub, l, r) -> Bop (Sub, derivate var l, derivate var r)
  | Bop (Mul, l, r) -> Bop (Add, Bop (Mul, derivate var l, r), Bop (Mul, l, derivate var r))
  | Bop (Div, l, r) -> Bop (Div, Bop (Sub, Bop (Mul, derivate var l, r), Bop (Mul, l, derivate var r)), Bop (Pow, r, Val 2.0))
  | Bop (Pow, l, r) when 
    (operation_contains (Var var) l) && (operation_contains (Var var) r |> not) -> 
      Bop (Mul, Bop (Mul, r, derivate var l), Bop (Pow, Var var, Bop (Sub, r, Val 1.)))
  (* | Bop (Pow, l, r) ->  *)
  (* TODO: Add exponential derivatives https://brilliant.org/wiki/derivatives-of-exponential-functions/ *)
  | Neg o -> Neg (derivate var o)
  | Fun (f, o) -> Bop (Mul, (match f, o with
    | ("sin", o) -> Fun ("cos", o)
    | ("asin", _) -> Bop (Div, Val 1.0, Bop (Pow, Bop (Sub, Val 1.0, Bop (Pow, Var var, Val 2.0)), Val 0.5))
    | ("cos", o) -> Neg (Fun ("sin", o))
    | ("acos", _) -> Bop (Div, Neg (Val 1.0), Bop (Pow, Bop (Sub, Val 1.0, Bop (Pow, Var var, Val 2.0)), Val 0.5))
    | ("tan", o) -> Bop (Div, Val 1.0, Bop (Pow, Fun ("cos", o), Val 2.0))
    | ("ln", o) -> Bop (Div, Val 1.0, o.(0))
    | ("log10", o) -> Bop (Div, Fun ("log10", [| Var "euler" |]), o.(0))
    | ("log2", o) -> Bop (Div, Fun ("log2", [| Var "euler" |]), o.(0))
    | _ -> raise (Missing_derivate f)), derivate var o.(0))
  | Var s -> if s = var then Val 1.0 else Val 0.0
  | Val _ -> Val 0.0
  | op -> raise (Missing_derivate (string_of_operation op));;