open Expression;;
module Plot = Oplot.Plt;;

let plot (op: operation_t) (var: string) (b: float) (e: float) = 
    let p = Plot.plot (fun x -> Hashtbl.add Expression.variables var x; Eval.eval Expression.variables op) b e in
    let a = Plot.axis b (Hashtbl.add Expression.variables var b; Eval.eval Expression.variables op) in
        Plot.display [ Plot.Color Plot.red; p; Plot.Color Plot.black; a ];;