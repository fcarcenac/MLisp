(* ********************************************************************** *)
(* TYPES DEFINITION *)


(** ************* PROPERTIES  ************      *)
module Pkey = struct
  type t = Symbol.t
  let compare = Symbol.compare
end

module PropList = Map.Make(Pkey)



(** ************* LISP CELLS & VALUES ************** *)
type subr =
  | F1 of (cell -> cell)
  | F2 of (cell -> cell -> cell)
  | Fn of (env -> cell -> cell)

and cell =
  | NIL
  | TRUE
  | Str of string
  | Nb of int
  | Port of port
  | Env of env
  | Subr of subr
  | Closure of (ext_cell Stack.t) list * (ext_cell Stack.t * cell) list * cell
  | Symb of Symbol.t
  | Path of cell * cell
  | Quote of cell
  | Unquote of cell
  | Quasiquote of cell
  | If of cell * cell * cell
  | Cons of cell * cell

and port =
  | Input of string * Lexing.lexbuf * in_channel option
  | Output of string * Buffer.t option * out_channel option 


and ext_cell = {
  value : cell ; 
  plist : cell PropList.t; 
  }

and v_stack = ext_cell Stack.t

(** **************** ENVIRONMENT ****************** *)

and t_values = v_stack array

and package = { 
  symbols        : Symbol.symbol_table ;
  e_name         : string ;
  id             : int ;
  father : package ; 
  dummy  : package }

and env = {
    values : t_values ;
    r1     : cell ;
    r2     : cell ;
    pkg    : package ; }

let dummy (_: ext_cell) =
  let rec e : package = { 
    id      = -1 ; 
    e_name  = "" ; 
    symbols = Symbol.create 0;
    father  = e ;
    dummy   = e ; }
  in
  e
