
(* ************************************************************************** *)
(* MODULES ALIASES *)

module E =Env
module PList = E.PropList

(* ************************************************************************** *)
(* EXCEPTIONS *)

exception Error

(* ************************************************************************** *)
(* TYPES DEFINITION *)

(* type t_env = ext_cell D.t_hashtable
and ext_env = {env : t_env ; parent : (ext_env ref) option }*)

type subr =
  | F1 of (cell -> cell)
  | F2 of (cell -> cell -> cell)
  | Fn of (ext_cell E.ext_t -> cell -> cell)

and cell =
  | TRUE
  | Str of string
  | Nb of int
  | Port of port
  | Env of ext_cell E.ext_t
  | Path of cell * cell
  | Quote of cell
  | Unquote of cell
  | Quasiquote of cell
  | If of cell * cell * cell
  | Symb of E.t_symbol
  | NIL
  | Subr of subr 
  | Cons of cell * cell
  | Closure of int list * (int * ext_cell) list * cell

(*
and ccell = { mutable car: cell ; mutable cdr: cell }
*)

and port =
  | Input of string * Lexing.lexbuf * in_channel option
  | Output of string * Buffer.t option * out_channel option

and ext_cell = {value : cell ; plist : cell PList.t}

exception Unknown of cell

(* ************************************************************************** *)
(* TYPE CHECKING functions *)

let is_num = function Nb _ -> true | _ -> false

let is_string = function Str _ -> true | _ -> false

let is_quote = function Quote _ -> true | _ -> false

let is_symb = function Symb _ -> true | _ -> false

let is_cons = function Cons (_,_) -> true | _ -> false

let is_null = function NIL -> true | _ -> false

let is_port = function Port _ -> true | _ -> false

let is_input_port = function Port(Input(_,_,_)) -> true | _ -> false
let is_input_file = function Port(Input(_,_,Some _)) -> true | _ -> false
let is_input_string = function Port(Input(_,_,None)) -> true | _ -> false

let is_output_port = function Port(Output(_,_,_)) -> true | _ -> false
let is_output_file = function Port(Output(_,None,Some _)) -> true | _ -> false
let is_output_string = function Port(Output(_,Some _,None)) -> true | _ -> false

let is_env = function Env _ -> true | _ -> false
let is_path = function Path _ -> true | _ -> false
let error msg = print_endline ("ERR: "^msg); raise Error
  

(* ************************************************************************** *)
(* PRETTY-PRINTING functions *)

let rec pp_cell x = function
  | NIL -> pp x
  | Cons(z,y) -> (pp x)^" "^(pp_cell z y)
  | y -> (pp x)^" . "^(pp y)

and pp = function
  | Nb n -> string_of_int n
  | Str s -> "\""^s^"\""
  | Port (Input(_,_,_)) -> "<input_port>"
  | Port (Output(_,_,_)) -> "<output_port>"
  | Env _ -> "<env>"
  | TRUE -> "#t"
  | Path(x,y) -> (pp x)^"."^(pp y)
  | Symb s -> E.name s
  | Quote c -> "'"^(pp c)
  | Unquote c -> ","^(pp c)
  | Quasiquote c -> "`"^(pp c)
  | If(c,t,e) -> "(if "^(pp c)^" "^(pp t)^" "^(pp e)^")"
  | NIL -> "()"
  | Cons(x,y) -> "("^(pp_cell x y)^")"
  | Subr _ -> "<subr>"
  | Closure _ -> "<closure>"

and pp' = function
  | Nb n -> string_of_int n
  | Str s -> s
  | Port (Input(_,_,_)) -> "<input port>"
  | Port (Output(_,_,_)) -> "<output port>"
  | Env _ -> "<env>"
  | TRUE -> "#t"
  | Path(x,y) -> (pp' x)^"."^(pp' y)
  | Symb s -> E.name s
  | Quote c -> "'"^(pp c)
  | Quasiquote c -> "`"^(pp c)
  | If(c,t,e) -> "(if "^(pp c)^" "^(pp t)^" "^(pp e)^")"
  | Unquote c -> ","^(pp c)
  | NIL -> "()"
  | Cons(x,y) -> "("^(pp_cell x y)^")"
  | Subr _ -> "<subr>"
  | Closure _ -> "<closure>"

and print t = print_string (pp t)


(* ************************************************************************** *)
(* ERROR HANDLING *)
(*
and error2 msg e = 
  print_endline ("ERR: "^msg^": "^(pp e)); 
  raise Error
*)
let error2 msg e = print_endline (msg^" - "^(pp e)); raise Error

let type_error msg e = 
  print_endline ("type error: ");
  error2 msg e

let binding_error msg e = 
  print_endline ("binding error: ");
  error2 msg e

(* ************************************************************************** *)
(* CELL ACCESSORS *)

let car = function Cons(c,_) -> c | t -> type_error "not a cons cell" t
let cdr = function Cons(_,l) -> l | t -> type_error "not a cons cell" t
let cadr = function Cons(_,Cons(c,_)) -> c | t -> type_error "not a cons cell" t
let cddr = function Cons(_,Cons(_,l)) -> l | t -> type_error "not a cons cell" t
let caddr = function 
  | Cons (_,(Cons(_,Cons(c,_)))) -> c 
  | t -> type_error "not a cons cell" t
let cdddr = function
  | Cons (_,(Cons(_,Cons(_,l)))) -> l
  | t -> type_error "not a cons cell" t


(* ************************************************************************** *)
(* Conversions *)

let int_of_cell = function Nb n -> n | t -> type_error "expected a num" t
let string_of_cell = function Str s -> s | t -> type_error "expected a string" t
let symb_of_cell = function Symb s -> s | t -> type_error "expected a symbol" t
let env_of_cell = function Env e -> e | t -> type_error "expected an environment" t
let fun_of_cell = function Subr f -> f | t -> type_error "expected a <subr>" t

let path_of_cell x =
  let rec path0 aux = function
    | Path (x, ((Path _) as y)) -> (path0 (x::aux) y)
    | Path (x, y) -> [x;y]
    | _ -> type_error "bad path expression" x
  in path0 [] x

(* ************************************************************************** *)
(* Ports Destructors *)
let get_input_port = function
  | Port(Input(x,y,z)) -> x,y,z
  | t -> type_error "expected to be an input port" t

let get_output_port = function
  | Port(Output(x,y,z)) -> x,y,z
  | t -> type_error "expected to be an output port" t


(* ************************************************************************** *)
(* Constructors *)

let mk_cons c l = Cons (c, l)

(* ************************************************************************** *)
(* Environment management *)
let init_env = (E.init 769 769 0 "MAIN": ext_cell E.ext_t)
let current_env = ref init_env 

let extend_local x y g = E.add g x {value = y ; plist = PList.empty}

let extend_global x y g =
  try 
    E.replace g x {(E.find g x) with value = y}
  with Not_found -> 
    E.add g x { value = y ; plist = PList.empty}

let full_lookup g x = (E.find_rec g x).value
let lookup g x = (E.find g x).value

let lookup_plist g x = (E.find g x).plist

let getprop g x p =
  let pl = lookup_plist g x in
  try 
    PList.find p pl
  with Not_found -> 
    binding_error "undefined property" (Symb p)

let addprop g x k v = 
  let c = E.find g x in 
  E.replace g x {c with plist = PList.add k v c.plist}

let rstprop g x =
  let c = E.find g x in 
  E.replace g x {c with plist = PList.empty}

let remprop g x k =
  let c = E.find g x in 
  E.replace g x {c with plist = PList.remove k c.plist}

let rec compile env x =
  let rec compile0 env x k = 
    match x with
    | TRUE | Nb _ | Str _ 
    | Port _ | Env _ | Closure _ | Subr _ 
    | NIL | Path (_,_) | Symb _ -> k x
    | Unquote y -> compile0 env y (fun z -> k (Unquote z))
    | Quote y -> compile0 env y (fun z -> k (Quote z)) 
    | Quasiquote y -> compile0 env y (fun z -> k (Quasiquote z))
    | Cons(Symb f,Cons(p,Cons(expr,NIL))) when f.E.name = "fun" ->
        compile0 env expr (fun x -> k (Closure(unbox p, [], x)))
    | Cons(Symb f,Cons(c,Cons(t,Cons(e,NIL)))) when f.E.name = "if" ->
        compile0 
          env c 
          (fun x -> 
            compile0 env t (fun y -> compile0 env e (fun z -> k (If(x,y,z)))))
    | Cons(Symb f,args) when f.E.name = "cond" -> 
        k (compile_cond env args)
    | Cons(car, cdr) -> 
        compile0 env car (fun x -> compile0 env cdr (fun y -> k (Cons(x,y))))
    | _ -> assert false
  in
  compile0 env x (fun x -> x)

and compile_cond env = function
  | NIL -> NIL
  | Cons (Cons(TRUE,Cons(t,NIL)), _) -> compile env t
  | Cons(Cons(c,Cons(t,NIL)),e) -> 
      If(compile env c,compile env t,compile_cond env e)
  | x -> error2 "invalid expression" x

and unbox = function
  | NIL -> []
  | Cons(Symb s,cdr) -> (s.E.i) :: (unbox cdr)
  | _ -> assert false
