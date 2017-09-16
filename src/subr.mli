val add_subr : Lsp.env -> string -> (Lsp.env -> Lsp.cell -> Lsp.cell) -> unit
val add_subr1 : Lsp.env -> string -> (Lsp.cell -> Lsp.cell) -> unit
val add_subr2 : Lsp.env -> string -> (Lsp.cell -> Lsp.cell -> Lsp.cell) -> unit
val init_global : unit -> unit
