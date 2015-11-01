val pp_pfx : string -> string -> string
val pp_par : string -> string
val pp_if : string -> string -> string -> string
val pp_cell : Lsp.cell -> Lsp.cell -> string
val pp : Lsp.cell -> string
val pp' : Lsp.cell -> string
exception Error
val error2 : string -> Lsp.cell -> 'a
val type_error : string -> Lsp.cell -> 'a
val create : int -> Lsp.ext_cell Stack.t array
val init : int -> int -> int -> string -> Lsp.env
val e_child : Lsp.env -> int -> Symbol.t -> Lsp.env
val env_name : Lsp.env -> string
(*val push : Lsp.env -> int -> Lsp.cell -> unit*)
(*val mov_r1 : Lsp.env -> Lsp.cell -> Lsp.env
val mov_r2 : Lsp.env -> Lsp.cell -> Lsp.env*)
(*val pop : Lsp.env -> int -> unit*)
(*val top : Lsp.env -> int -> Lsp.cell*)
val replace : Lsp.env -> Symbol.t -> Lsp.ext_cell -> unit
val lookup : Lsp.env -> Symbol.t -> Lsp.env
val lookup_plist : Lsp.env -> Symbol.t -> Lsp.cell Lsp.PropList.t
val getprop : Lsp.env -> Symbol.t -> Lsp.PropList.key -> Lsp.cell
val addprop : Lsp.env -> Symbol.t -> Lsp.PropList.key -> Lsp.cell -> unit
val rstprop : Lsp.env -> Symbol.t -> unit
val remprop : Lsp.env -> Symbol.t -> Lsp.PropList.key -> unit
val extend_global : Symbol.t -> Lsp.cell -> Lsp.env -> unit
(*val e_inherit : Lsp.env -> Symbol.t -> Lsp.env*)
val symbols : Lsp.env -> Symbol.t list
val symbol : Lsp.env -> string -> Symbol.t
val int_of_cell : Lsp.cell -> int
val string_of_cell : Lsp.cell -> string
val symb_of_cell : Lsp.cell -> Symbol.t
val env_of_cell : Lsp.cell -> Lsp.env
val fun_of_cell : Lsp.cell -> Lsp.subr
val get_input_port : Lsp.cell -> string * Lexing.lexbuf * in_channel option
val get_output_port :
  Lsp.cell -> string * Buffer.t option * out_channel option
