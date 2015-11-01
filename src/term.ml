
(* ********************************************************************** *)
(* MODULES ALIASES *)
module E =Env
module PList = E.PropList

(* ********************************************************************** *)
(* EXCEPTIONS *)

exception Error

(* ********************************************************************** *)
(* TYPES DEFINITION *)

type env = (ext_cell, cell) E.ext_t

and subr =
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
  | Closure of int list * (int * ext_cell) list * cell
  | Symb of E.t_symbol
  | Path of cell * cell
  | Quote of cell
  | Unquote of cell
  | Quasiquote of cell
  | If of cell * cell * cell
(*  
  | Get of ext_cell E.ext_t * int
  | Set of ext_cell E.ext_t * int * cell
*)
  | Cons of cell * cell

(*
and ccell = { mutable car: cell ; mutable cdr: cell }
*)

and port =
  | Input of string * Lexing.lexbuf * in_channel option
  | Output of string * Buffer.t option * out_channel option

and ext_cell = {
  value : cell ; 
  plist : cell PList.t; 
  }

exception Unknown of cell

(* ********************************************************************** *)
(* TYPE CHECKING functions *)

let is_num = function 
  | Nb _ -> true 
  | _ -> false

let is_string = function 
  | Str _ -> true 
  | _ -> false

let is_quote = function 
  | Quote _ -> true 
  | _ -> false

let is_symb = function 
  | Symb _ -> true 
  | _ -> false

let is_cons = function 
  | Cons (_,_) -> true 
  | _ -> false

let is_port = function 
  | Port _ -> true 
  | _ -> false

let is_input_port = function 
  | Port(Input(_,_,_)) -> true 
  | _ -> false

let is_input_file = function 
  | Port(Input(_,_,Some _)) -> true 
  | _ -> false

let is_input_string = function 
  | Port(Input(_,_,None)) -> true 
  | _ -> false

let is_output_port = function 
  | Port(Output(_,_,_)) -> true 
  | _ -> false

let is_output_file = function 
  | Port(Output(_,None,Some _)) -> true 
  | _ -> false

let is_output_string = function 
  | Port(Output(_,Some _,None)) -> true 
  | _ -> false

let is_env = function 
  | Env _ -> true 
  | _ -> false

let is_path = function 
  | Path _ -> true 
  | _ -> false

let error msg = print_endline ("ERR: "^msg); raise Error
  

(* ********************************************************************** *)
(* PRETTY-PRINTING functions *)

let pp_pfx p x = p ^ x 
let pp_par x = "(" ^ x ^ ")"
let pp_if x y z = 
  let s = "if" ^ x ^ " " ^ y ^ " " ^ z in pp_par s

let rec pp_cell x = function
  | NIL -> pp x
  | Cons(z,y) -> (pp x) ^ " " ^ (pp_cell z y)
  | y -> (pp x) ^ " . " ^ (pp y)

and pp = function
  | Nb n -> string_of_int n
  | Str s -> "\"" ^ s ^ "\""
  | Port (Input(_,_,_)) -> "<input_port>"
  | Port (Output(_,_,_)) -> "<output_port>"
  | Env _ -> "<env>"
  | TRUE -> "#t"
  | Path(x,y) -> (pp x) ^ "." ^ (pp y)
  | Symb s -> E.name s
  | Quote c -> "'" ^ (pp c)
  | Unquote c -> "," ^ (pp c)
  | Quasiquote c -> "`" ^ (pp c)
  | If(c,t,e) -> 
      "(if " ^ (pp c) ^ " " ^ (pp t) ^ " " ^ (pp e) ^ ")"
  | NIL -> "()"
  | Cons(x,y) -> "(" ^ (pp_cell x y) ^ ")"
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

(* Constructor *)
let mk_cell v = {value = v ; plist = PList.empty}

let dummy = mk_cell NIL

(* ********************************************************************** *)
(* ERROR HANDLING *)

let error2 msg e = 
  print_endline (msg^" - "^(pp e)); 
  raise Error

let type_error msg e = 
  print_endline ("type error: ");
  error2 msg e

let binding_error msg e = 
  print_endline ("binding error: ");
  error2 msg e

(* ************************************************************************** *)
(* Conversions *)

let int_of_cell = function 
  | Nb n -> n 
  | t -> type_error "expected a num" t

let string_of_cell = function
  | Str s -> s 
  | t -> type_error "expected a string" t

let symb_of_cell = function 
  | Symb s -> s 
  | t -> type_error "expected a symbol" t

let env_of_cell = function 
  | Env e -> e 
  | t -> type_error "expected an environment" t

let fun_of_cell = function 
  | Subr f -> f 
  | t -> type_error "expected a <subr>" t

(*
let path_of_cell x =
  let rec path0 aux = function
    | Path (x, ((Path _) as y)) -> (path0 (x::aux) y)
    | Path (x, y) -> [x;y]
    | _ -> type_error "bad path expression" x
  in path0 [] x
*)

(* ************************************************************************** *)
(* Ports Destructors *)
let get_input_port = function
  | Port(Input(x,y,z)) -> x,y,z
  | t -> type_error "expected to be an input port" t

let get_output_port = function
  | Port(Output(x,y,z)) -> x,y,z
  | t -> type_error "expected to be an output port" t


(* ************************************************************************** *)
(* Environment management *)
let init_env =
  (*(E.init 999331 999331 0 "MAIN" (mk_cell NIL) : ext_cell E.ext_t)*)

  (E.init 769 769 0 "MAIN" NIL (mk_cell NIL) : env)
let current_env = ref init_env 

let extend_global x y g =
  let c = 
    try
      { (E.find_rec g x) with value = y }
    with Not_found -> { value = y ; plist = PList.empty }
    in 
    E.replace g x c

let push (e: (ext_cell,cell) E.ext_t) i (x:cell) =
  let slot = Array.unsafe_get e.E.values i in
  Stack.push 
    {value = x ; plist = PList.empty} 
    slot

let mov_r1 e x = { e with E.r1 = x} 

let mov_r2 e x = { e with E.r2 = x} 

let pop (e: (ext_cell,cell) E.ext_t) i = 
  Stack.pop (Array.unsafe_get e.E.values i)

let top e i =
  (Stack.top (Array.unsafe_get e.E.values i)).value

let full_lookup e x =
  (E.find_rec e x).value

let lookup e x =
  match (E.find e x).value with
  | Env e -> e
  | _ -> type_error "expected an <env> value" (Symb x)

let lookup_plist e x = 
  (E.find_rec e x).plist

let getprop e x p =
  let pl = lookup_plist e x in
  try PList.find p pl
  with Not_found -> 
    binding_error "undefined property" (Symb p)

let addprop e x k v = 
  let c = E.find_rec e x in 
  E.replace e x {c with plist = PList.add k v c.plist}

let rstprop e x =
  let c = E.find_rec e x in 
  E.replace e x {c with plist = PList.empty}

let remprop e x k =
  let c = E.find_rec e x in 
  E.replace e x {c with plist = PList.remove k c.plist}

let add x y =
  match x, y with
  | Nb x, Nb y -> Nb (x+y)
  | _ -> type_error "expectd a number" (Cons(x, Cons (y, NIL)))

let rec unbox = function
  | NIL -> []
  | Cons(Symb s,cdr) -> (s.E.i) :: (unbox cdr)
  | _ -> assert false

(*
let rec compile env x =
  let rec compile0 env x k = 
    match x with
    | TRUE | Nb _ | Str _ 
    | Port _ | Env _ 
    | NIL
    | Closure _ | Subr _ 
    | Path (_,_) | Symb _ -> k x
    | Unquote y -> compile0 env y (fun z -> k (Unquote z))
    | Quote y -> compile0 env y (fun z -> k (Quote z)) 
    | Quasiquote y -> compile0 env y (fun z -> k (Quasiquote z))
    | Cons(Symb { E.name = "fun"  ; _ }, Cons(p,Cons(expr,NIL))) ->
        compile0 env expr (fun x -> k (Closure(unbox p, [], x)))
    | Cons(Symb { E.name = "if" ; _ },Cons(c,Cons(t,Cons(e,NIL)))) ->
        compile0 
          env c 
          (fun x -> 
            compile0 env t (fun y -> compile0 env e (fun z -> k (If(x,y,z)))))
    | Cons(Symb {E.name = "cond" ; _ },args) -> 
        k (compile_cond env args)
    | Cons(car, cdr) -> 
        compile0 env car (fun x -> compile0 env cdr (fun y -> k (Cons(x,y))))
    | _ -> assert false
  in
  compile0 env x (fun x -> x)
*)

(*
let rec arity = function
  | NIL -> 0
  | Cons (_, cdr) -> succ (arity cdr)
  | _ -> assert false*)

let compile (env: (ext_cell,cell) E.ext_t) x =
let rec compile0 env x =
  match x with
  | TRUE  -> x
  | Nb _  -> x
  | Str _  -> x
  | Port _  -> x
  | Env _ -> x
  | NIL  -> x
  | Closure _  -> x
  | Subr _ -> x
  | Path (_,_) -> x
  | Symb _ -> x 
  | Unquote y -> Unquote (compile0 env y)
  | Quote y -> Quote (compile0 env y) 
  | Quasiquote y -> Quasiquote (compile0 env y)
  | Cons(Symb { E.name = "fun" ; _ }, 
         Cons(p,Cons(expr,NIL))) ->
      Closure(unbox p, [], compile0 env expr)
  | Cons(Symb { E.name = "quote" ; _ }, Cons(e, NIL)) ->
      Quote (compile0 env e)
  | Cons(Symb { E.name = "if" ; _ },
         Cons(c,Cons(t,Cons(e,NIL)))) ->
      If (compile0 env c, compile0 env t, compile0 env e)
  | Cons(Symb {E.name = "cond" ; _ },args) -> 
      compile_cond env args
  | Cons(car, cdr) -> Cons (compile0 env car, compile0 env cdr)
  | _ -> assert false

and compile_cond env = function
  | NIL -> NIL
  | Cons (Cons(TRUE,Cons(t,NIL)), _) -> compile0 env t
  | Cons(Cons(c,Cons(t,NIL)),e) -> 
      If(compile0 env c,compile0 env t,compile_cond env e)
  | x -> error2 "invalid expression" x
in
compile0 env x

let car = function 
  | Cons(c,_) -> c 
  | t -> error2 "not a cons cell" t

let cdr = function 
  | Cons(_,c) -> c 
  | t -> error2 "not a cons cell" t

let cadr t = car (cdr t)



(* ********************************************************************** *)
(* Quasiquoting/Evaluation/Application *)

(* 
NOTE: 
  quasiquote should handle explicit levels (in order to be able to combine
  multiple quasiquote/unquote
  ``(,x y z) which evaluates to (x '(y z))
*)

let rec quasiquote env n y =
    match y with
    (*| _ when is_atom y -> y*)
    | NIL | TRUE 
    | Subr _ | Closure _ 
    | Str _ | Nb _ | Port _ | Env _ -> y
    | Symb _ | Quote _ | Path (_,_) -> (*y*)
        if n>1 then Quote (quasiquote env (n-1) y) else y
    | Cons(x, l) -> 
        Cons(quasiquote env n x, quasiquote env n l)
    | If(c,t,e) -> 
        If (quasiquote env n c, 
            quasiquote env n t, 
            quasiquote env n e)
    | Unquote z -> unquote env (n-1) z
    | Quasiquote y -> quasiquote env (n+1) y

and unquote env n y = 
  if n <> 0 then quasiquote env n y else eval env y

(*
evalk env k x =
  match x with
  | TRUE | Nb _ | Str _ | NIL | Port _ | Env _ | Subr _ | Closure _ -> k x
  | Symb s -> k (full_lookup env s)
  | Path _ -> evalk_path env k x
  | If (c,t,e) ->
      eval env 
        (function 
          | TRUE -> eval env k t 
          | Nil  -> eval env k e
          | _    -> assert false) c
  | Quote y -> k y
  | Quasiquote y -> k (quasiquote env 1 y)
  | Cons (car, cdr) ->
      begin
        match car with
        | Symb s -> app_evalk env k cdr (full_lookup env s)
        | Path as f ->
            evalk_path env (fun x -> app_evalk env k cdr x) f
        | _ -> app_evalk env k cdr car
      end

let evalk_path env k = function
  | Symb s -> k (full_lookup env s)
  | Path (Symb s, p) -> 
      evalk_path (lookup env s) k p
  | x -> type_error "expected a path or a symbol"

let app_evalk env k args = function
  | Subr f -> 

*)
and eval env x = 
  match x with
  | TRUE -> x
  | Nb _ -> x
  | Str _-> x
  | NIL -> x
  | Port _ -> x
  | Env _ -> x
  | Subr _ -> x
  | Closure _ -> x
  | Symb s -> full_lookup env s 
  | Path _ -> eval_path env x
  | If(c,t,e) ->
      let c = if (eval env c) == NIL then e else t in
      eval env c
  | Quote y -> y
  | Quasiquote y -> quasiquote env 1 y
  | Cons(car, cdr) -> 
      (match car with
      | Symb s -> app_eval env cdr (full_lookup env s)
      | Path _ -> eval_app_path env cdr car
      | _      -> app_eval env cdr car)
  | _ -> (*x*)
      assert false

and eval_path env = function
  | Symb x         -> full_lookup env x
  | Path(Symb x,y) -> eval_path (lookup env x) y
  | x              -> type_error "expected a path or a symbol" x

and eval_app_path env args = function
  | Symb s           -> app_eval env args (full_lookup env s)
  | Path (Symb p, s) -> eval_app_path (lookup env p) args s
  | x                -> app_eval env args x

and app_eval env args = function
  | Subr f -> 
      (*app_subr env args f*)
      begin
        match f, args with
        | F1 f, Cons(x,NIL) -> 
            f (eval env x)
        | F2 f, Cons(x, Cons(y,NIL)) -> 
            f (eval env x) (eval env y)
        | Fn f, _ -> 
            f env args
        | _, _ -> 
            assert false
      end
  | Closure(params, l, expr) ->
      begin
        match l with
        | [] ->
            begin
              match params, args with
              | [], NIL                      -> 
                  eval env expr 
              | [p], Cons(x,NIL)             -> 
                  push env p (eval env x);
                  let expr = eval env expr in
                  pop env p;
                  expr
              | [p1;p2], Cons(x,Cons(y,NIL)) -> 
                  push env p1 (eval env x);
                  push env p2 (eval env y);
                  let expr = eval env expr in
                  pop env p1;
                  pop env p2;
                  expr
              | _                            ->
                  applyN env expr [] params args
            end
        | l -> applyN env expr l params args
      end
  | x -> type_error "expected to be an 'fun'" x

and applyN env expr l params args =
  match params, args with
  | [], NIL -> 
      apply env (eval env expr) l
  | _ , NIL -> Closure(params, l, expr)
  | i :: y, Cons(a,b) ->
      push env i (eval env a);
      applyN env expr ((i,dummy)::l) y b 
  | [], _ -> error "too many arguments" 
  | _,_ -> assert false


and apply env exp = function
  (* pop arguments values *)
  | [] -> exp
  | (id,_) :: tl ->
      pop env id;
      apply env exp tl
