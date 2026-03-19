open Expression;;

let help_short = "SOLVE expression variable [value] // Solve the expression to equal 0";;

let help_long = "Solve command: Find a zero of a given expression
Syntax: SOLVE expression variable [value]
With:
\t- Expression: The expression to be solved
\t- Variable: The variable to solve the expression, the rest of variables are treated like constants
\t- Value: Optional, the value from which the solver will start trying";;

let find_zero (o: operation_t) (v: string) (i: float) (e: float): float = 
  let rec newton (d: operation_t) (x: float) (depth: int): float = 
    add_var v x;
    let fx = Eval.eval variables o in
    let dfx = Eval.eval variables d in
    if Float.abs fx <= e || depth > 10 then x else newton d (x -. fx /. dfx) (depth+1) in
  try
    newton (Derivate.derivate v o) i 0
  with 
    Missing_derivate _ -> 0.;;
