type expr_t = Op of Operation.operation_t
            | FunDef of (string * (string list) * Operation.operation_t)
            | VarDef of (string * float)

