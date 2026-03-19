open Expression;;
module Plot = Oplot.Plt;;

let help_short = "PLOT expression variable begin end // Plot the variable in the specified range";;

let help_long = "Plot command: Plot a given expression
Syntax: PLOT expression variable begin end
With:
\t- Expression: The expression to be plotted
\t- Variable: The variable of the expression to plot
\t- Beging: Starting point to plot
\t- End: Last value to plot";;

let plot (op: operation_t) (var: string) (b: float) (e: float) = 
    let p = Plot.plot (fun x -> Expression.add_var var x; Eval.eval Expression.variables op) b e in
    let a = Plot.axis b (Expression.add_var var b; Eval.eval Expression.variables op) in
        Plot.display [ Plot.Color Plot.red; p; Plot.Color Plot.black; a ];;
