
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

type cell =
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
  | Fun 
  | Symb of E.t_symbol
  | NIL
  | Subr of (ext_cell E.ext_t -> cell -> cell)
  | Cons of cell * cell

and port =
  | Input of string * Lexing.lexbuf * in_channel option
  | Output of string * Buffer.t option * out_channel option

and ext_cell = {value : cell ; plist : cell PList.t}

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

let rec pp_cell = function
  | Cons(x,NIL) -> pp x
  | Cons(x,(Cons(_,_) as y)) -> (pp x)^" "^(pp_cell y)
  | Cons(x,y) -> (pp x)^" . "^(pp y)
  | _ -> assert false

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
  | Fun -> "fun"
  | NIL -> "()"
  | Cons(_,_) as x -> "("^(pp_cell x)^")"
  | Subr _ -> "<subr>"

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
  | Fun -> "fun"
  | NIL -> "()"
  | Cons(x,y) when (not (is_cons y)) -> "("^(pp x)^" . "^(pp y)^")" 
  | Cons(_,_) as x -> "("^(pp_cell x)^")"
  | Subr _ -> "<subr>"

and print t = print_string (pp t)


(* ************************************************************************** *)
(* ERROR HANDLING *)

and error2 msg e = 
  print_endline ("ERR: "^msg^": "^(pp e)); 
  raise Error


(* ************************************************************************** *)
(* CELL ACCESSORS *)

and car = function Cons(c,_) -> c | t -> error2 "not a cons cell" t
and cdr = function Cons(_,l) -> l | t -> error2 "not a cons cell" t
and cadr = function Cons(_,Cons(c,_)) -> c | t -> error2 "not a cons cell" t
and cddr = function Cons(_,Cons(_,l)) -> l | t -> error2 "not a cons cell" t
and caddr = function 
  | Cons (_,(Cons(_,Cons(c,_)))) -> c 
  | t -> error2 "not a cons cell" t
and cdddr = function
  | Cons (_,(Cons(_,Cons(_,l)))) -> l
  | t -> error2 "not a cons cell" t


(* ************************************************************************** *)
(* Conversions *)

let error2 msg e = print_endline (msg^": "^(pp e)); raise Error

let int_of_cell = function Nb n -> n | t -> error2 "not a num" t
let string_of_cell = function Str s -> s | t -> error2 "not a string" t
let symb_of_cell = function Symb s -> s | t -> error2 "not a symbol" t
let env_of_cell = function Env e -> e | t -> error2 "not a environment" t
let fun_of_cell = function
  | Subr f -> f 
  | _ -> failwith "fun_of_cell - internal error"

let path_of_cell x =
  let rec path0 aux = function
    | Path (x, ((Path _) as y)) -> (path0 (x::aux) y)
    | Path (x, y) -> [x;y]
    | _ -> error2 "not a path" x
  in path0 [] x

(* ************************************************************************** *)
(* Ports Destructors *)
let get_input_port = function
  | Port(Input(x,y,z)) -> x,y,z
  | t -> error2 "expected to be an input port" t

let get_output_port = function
  | Port(Output(x,y,z)) -> x,y,z
  | t -> error2 "expected to be an output port" t


(* ************************************************************************** *)
(* Constructors *)

let mk_cons c l = Cons (c, l)

(* ************************************************************************** *)
(* Environment management *)
let init_env = (E.init 769 769 0 "MAIN": ext_cell E.ext_t)
let current_env = ref init_env 
let copy_env = E.copy

let extend_local x y g = E.add g x {value = y ; plist = PList.empty}

let extend_global x y g =
  try E.replace g x {(Env.find g x) with value = y}
  with Not_found -> E.add g x { value = y ; plist = PList.empty}

let lookup g x = (E.find g x).value

let lookup_plist g x = (E.find g x).plist

let getprop g x p =
  let pl = lookup_plist g x in
  try PList.find p pl
  with Not_found -> error2 "undefined property" (Symb p)

let addprop g x k v = 
  let c = Env.find g x in 
  E.replace g x {c with plist = PList.add k v c.plist}

let rstprop g x =
  let c = Env.find g x in 
  E.replace g x {c with plist = PList.empty}

let remprop g x k =
  let c = Env.find g x in 
  E.replace g x {c with plist = PList.remove k c.plist}

(* ************************************************************************** *)
(* Quasiquoting/Evaluation/Application *)

(* 
NOTE: 
  quasiquote should handle explicit levels (in order to be able to combine
  multiple quasiquote/unquote
  ``(,x y z) which evaluates to (x '(y z))
*)

let rec quasiquote env n y =
    match y with
    | NIL | TRUE | Fun | Subr _ 
    | Str _ | Nb _ | Port _ | Env _ -> y
    | Symb _ | Quote _ | Path (_,_) -> (*y*)
        if n>1 then Quote (quasiquote env (n-1) y) else y
    | Cons(Fun, l) -> Cons(Fun, quasiquote env n l)
    | Cons(x, l) -> Cons(quasiquote env n x, quasiquote env n l)
    | If(c,t,e) -> 
        If(quasiquote env n c, quasiquote env n t, quasiquote env n e)
    | Unquote z -> unquote env (n-1) z
    | Quasiquote y -> quasiquote env (n+1) y

and unquote env n y = 
  if n != 0 then quasiquote env n y else let _,e = eval_c env y in e

and unbox = function
  | NIL -> []
  | Cons(car,cdr) -> car :: (unbox cdr)

and compile_cond env = function
  | NIL -> NIL
  | Cons (Cons(TRUE,Cons(t,NIL)), _) -> compile env t
  | Cons(Cons(c,Cons(t,NIL)),e) -> If(c,compile env t,compile_cond env e)
  | x -> error2 "invalid expression" x
      
and compile env x =
  match x with
  | NIL | TRUE | Nb _ | Str _ | Port _ | Env _ | Fun -> x 
  | Symb y -> (try lookup env y with Not_found -> x)
  | Path (_,_) -> (try snd (eval_path env x) with Not_found -> x)
  | Unquote y -> Unquote (compile env y)
  | Quote y -> Quote (compile env y) 
  | Quasiquote y -> Quasiquote (compile env y)
  | Cons(Symb f,args) when f = Env.symbol env "cond" -> compile_cond env args
  | Cons(car, cdr) -> Cons (compile env car, compile env cdr)
  | If(c,t,e) -> If(compile env c, compile env t, compile env e)

and eval_c env x =
  try
    match x with
    | NIL | TRUE | Nb _ | Str _ | Port _ | Env _ -> env, x
    | Symb x -> env, lookup env x
    | Path(_,_) -> eval_path env x
    | Quote y -> env, y
    | Quasiquote y -> env, quasiquote env 1 y
    | If(c,t,e) ->
        begin
          match eval_c env c with
          | _, TRUE -> eval_c env t 
          | _, _ -> eval_c env e
        end
    | Cons(car, cdr) -> env, app_eval env car cdr
    | _ -> error2 "invalid expression" x
  with 
  | Error -> 
    begin
      print_endline ("error when evaluating ... "^(pp x));
      env,NIL
    end
  | Not_found ->
      begin
        print_endline ("unknown identifier: "^(pp x));
        env,NIL
      end

and eval_path env = function
  | Symb x -> env, lookup env x
  | Path(Symb x,y) -> eval_path (env_of_cell (lookup env x)) y
  | x -> error2 "incorrect symbol" x 

and app_eval env f args =
  match f with
  | Subr cf -> cf env args 
  | Path(_,_) -> let ne, f = eval_path env f in app_eval ne f args 
  | Symb s -> app_eval env (lookup env s) args
  | Cons(Fun,Cons(params,Cons(expr,NIL))) -> 
      bind_eval env Fun expr params args
  | Fun -> Cons(f,args)
  | _ -> Cons(f,args)

and eval_list env = function
  | NIL -> []
  | Cons (car,cdr) -> 
      let r = eval_c env car in r :: (eval_list env cdr)

and bind_eval env c exp p a =
  let rec f acc p a =
    match c,p,a with
    | _,NIL,NIL -> bind env exp acc
    | _,NIL, _ -> error "too many arguments"
    | Fun, _, NIL -> error "too few arguments" 
    | _,Cons(Symb x,y),Cons(a,b) -> 
        let c = {value = eval env a ; plist = PList.empty} in
        f ((x.E.i,c)::acc) y b
    | _,_,_ -> assert false in
  f [] p a

and bind env exp l =
  let rec f l acc =
    match l with
    | [] -> apply env exp acc
    | (id,c)::tl ->
        E.O.A.set env.E.values id (c :: (E.O.A.get env.E.values id));
        f tl (id::acc) in
  f l []

and apply env exp l =
  let _, ne = eval_c env exp in pop_list env ne l

and pop_list env e = function
  | [] -> e
  | id::tl ->
      (match E.O.A.get env.E.values id with
      | _::l -> E.O.A.set env.E.values id l
      | _ -> assert false);
      pop_list env e tl
        
and eval env x = snd (eval_c env x)
