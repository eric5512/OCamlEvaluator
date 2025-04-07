open Expression;;

(* TODO: Add non-trivial simplification *)

let simplify expr = 
  let rec aux = function
      | Bop (op, Val l, Val r) -> (Val ((bop_to_op op) l r), true)

      (* generic add *)
      | Bop (Add, l, Val 0.0) -> (l, true)
      | Bop (Add, Val 0.0, r) -> (r, true)
      | Bop (Add, Neg l, r) when l = r -> (Val 0.0, true)
      | Bop (Add, l, Neg r) when l = r -> (Val 0.0, true)
      
      (* add to mul rules *)
      | Bop (Add, Var l, Var r) when l = r -> (Bop (Mul, Val 2.0, Var l), true)
      | Bop (Add, Var l, Bop (Mul, Val v, Var r)) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Add, Var l, Bop (Mul, Var r, Val v)) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Add, Bop (Mul, Var l, Val v), Var r) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Add, Bop (Mul, Val v, Var l), Var r) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Add, Bop (Mul, Val vl, Var l), Bop (Mul, Val vr, Var r)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Add, Bop (Mul, Var l, Val vl), Bop (Mul, Val vr, Var r)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Add, Bop (Mul, Var l, Val vl), Bop (Mul, Var r, Val vr)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Add, Bop (Mul, Val vl, Var l), Bop (Mul, Var r, Val vr)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)

      (* generic sub *)
      | Bop (Sub, l, Val 0.0) -> (l, true)
      | Bop (Sub, Val 0.0, r) -> (Neg r, true)
      | Bop (Sub, l, Neg r) -> (Bop (Add, l, r), true)
      | Bop (Sub, Neg l, r) -> (Neg (Bop (Add, l, r)), true)
      | Bop (Sub, l, r) when l = r -> (Val 0.0, true)
      
      (* sub to mul rules *)
      | Bop (Sub, Var l, Bop (Mul, Val v, Var r)) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Sub, Var l, Bop (Mul, Var r, Val v)) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Sub, Bop (Mul, Var l, Val v), Var r) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Sub, Bop (Mul, Val v, Var l), Var r) when l = r -> (Bop (Mul, Val (v +. 1.0), Var l), true)
      | Bop (Sub, Bop (Mul, Val vl, Var l), Bop (Mul, Val vr, Var r)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Sub, Bop (Mul, Var l, Val vl), Bop (Mul, Val vr, Var r)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Sub, Bop (Mul, Var l, Val vl), Bop (Mul, Var r, Val vr)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)
      | Bop (Sub, Bop (Mul, Val vl, Var l), Bop (Mul, Var r, Val vr)) when l = r -> (Bop (Mul, Val (vl +. vr), Var l), true)

      (* generic mul *)
      | Bop (Mul, l, Val 1.0) -> (l, true)
      | Bop (Mul, Val 1.0, r) -> (r, true)
      | Bop (Mul, _, Val 0.0) -> (Val 0.0, true)
      | Bop (Mul, Val 0.0, _) -> (Val 0.0, true)
      
      (* mul to pow rules *)
      | Bop (Mul, Var l, Var r) when l = r -> (Bop (Pow, Var l, Val 2.0), true)
      | Bop (Mul, Bop (Pow, Var l, Val v), Var r) when l = r -> (Bop (Pow, Var l, Val (v +. 1.0)), true)
      | Bop (Mul, Var l, Bop (Pow, Var r, Val v)) when l = r -> (Bop (Pow, Var l, Val (v +. 1.0)), true)
      | Bop (Mul, Bop (Pow, Var l, Val v), Var r) when l = r -> (Bop (Pow, Var l, Val (v +. 1.0)), true)
      | Bop (Mul, Bop (Pow, Var l, Val vl), Bop (Pow, Var r, Val vr)) when l = r -> (Bop (Pow, Var l, Val (vl +. vr)), true)

      (* generic div *)
      | Bop (Div, l, Val 1.0) -> (l, true)
      | Bop (Div, _, Val 0.0) -> (Val infinity, true)
      | Bop (Div, Val 0.0, _) -> (Val 0.0, true)
      | Bop (Div, l, r) when l = r -> (Val 1.0, true)
      
      (* div to pow rules *)
      | Bop (Div, Var l, Bop (Pow, Val v, Var r)) when l = r -> (Bop (Pow, Val (v +. 1.0), Var l), true)
      | Bop (Div, Var l, Bop (Pow, Var r, Val v)) when l = r -> (Bop (Pow, Val (v +. 1.0), Var l), true)
      | Bop (Div, Bop (Pow, Var l, Val v), Var r) when l = r -> (Bop (Pow, Val (v +. 1.0), Var l), true)
      | Bop (Div, Bop (Pow, Var l, Val vl), Bop (Pow, Var r, Val vr)) when l = r -> (Bop (Pow, Val (vl +. vr), Var l), true)
      
      (* generic pow *)
      | Bop (Pow, l, Val 1.0) -> (l, true)
      | Bop (Pow, Val 1.0, _) -> (Val 1.0, true)
      | Bop (Pow, _, Val 0.0) -> (Val 1.0, true)
      | Bop (Pow, Val 0.0, _) -> (Val 0.0, true)

      | Bop (op, l, r) -> let (l, c1) = aux l in let (r, c2) = aux r in (Bop (op, l, r), c1 || c2)
      
      | Neg o -> (match o with
        | Neg no -> (no, true)
        | _ -> let (o, c) = aux o in (Neg o, c))
      
      | Fun (f, o) -> 
        let acc = 
          Array.fold_left 
            (fun (x1, y1) (x2, y2) -> ( x2::x1, y1 || y2))
            ([], false)
            (Array.map aux o) in 
        (Fun (f, fst acc |> Array.of_list), snd acc)
      
      | _ as c -> (c, false) in
    let repeat = ref true in
    let op = ref expr in
    while !repeat do
      let (o, r) = aux !op in
        op := o; repeat := r;
    done;
    !op;;