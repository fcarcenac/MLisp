val compile : Lsp.env -> Lsp.cell -> Lsp.cell

val eval : Lsp.env -> Lsp.cell -> Lsp.cell

val print : Lsp.cell -> unit

val error: string -> Lsp.cell

val to_stack_list : Lsp.env -> Lsp.cell -> (Lsp.ext_cell Stack.t) list

val push : Lsp.ext_cell Stack.t -> Lsp.cell -> unit
val popN: ((Lsp.ext_cell Stack.t) * Lsp.cell) list -> unit

val is_num : Lsp.cell -> bool
val is_output_port : Lsp.cell -> bool
val is_string : Lsp.cell -> bool
val is_env : Lsp.cell -> bool
val is_symb : Lsp.cell -> bool
val is_input_file : Lsp.cell -> bool
val is_output_string : Lsp.cell -> bool
val is_output_file : Lsp.cell -> bool
val is_input_port : Lsp.cell -> bool
val add : Lsp.env -> int -> Lsp.cell -> Lsp.cell
(*
val is_quote : Lsp.cell -> bool
val is_cons : Lsp.cell -> bool
val is_port : Lsp.cell -> bool
val is_input_string : Lsp.cell -> bool
*)



