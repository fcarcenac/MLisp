
(* ********************************************************************** *)
(* MODULES ALIASES *)
open Lsp
module S = Symbol

(* ********************************************************************** *)
(* TYPE CHECKING functions *)


let is_num = function 
  | Nb _ -> true 
  | _ -> false

let is_output_port = function 
  | Port(Output(_,_,_)) -> true 
  | _ -> false
 
let is_string = function 
  | Str _ -> true 
  | _ -> false

let is_env = function 
  | Env _ -> true 
  | _ -> false

let is_symb = function 
  | Symb _ -> true 
  | _ -> false

let is_input_file = function 
  | Port(Input(_,_,Some _)) -> true 
  | _ -> false

let is_output_string = function 
  | Port(Output(_,Some _,None)) -> true 
  | _ -> false

let is_output_file = function 
  | Port(Output(_,None,Some _)) -> true 
  | _ -> false

let is_input_port = function 
  | Port(Input(_,_,_)) -> true 
  | _ -> false

let error msg = print_endline ("ERR: "^msg); raise LspUtils.Error

let print t = print_string (LspUtils.pp t)

(* ************** ENVIRONMENT LOOKUP ******************* *)
let rec find_rec d i =
  let stack = Array.unsafe_get d.values i in
  if stack.Stack.c == []
  then 
    begin
      let father = d.pkg.father in
      if father == d.pkg.dummy then raise Not_found
      else find_rec {d with pkg = father} i
    end
  else Stack.top stack

let full_lookup e x = (find_rec e x.S.i).value

let lookup e ({S.name = _ ; S.i = i ; S.scope = _} as x) =
  let stack = Array.unsafe_get e.values i in
  if stack.Stack.c == []
  then raise Not_found
  else 
    match (Stack.top stack).value with
    | Env e -> e
    | _ -> LspUtils.type_error "expected an <env> value" (Symb x)

(* **************** STACK OPERATIONS ******************* *)
(*let push e i x =
  Stack.push 
    {value = x ; plist = PropList.empty} 
    (Array.unsafe_get e.values i)

let pop e i = Stack.pop (Array.unsafe_get e.values i)*)

let push s x = Stack.push {value = x ; plist = PropList.empty} s
let pop s = Stack.pop s

(* ************************************************************************ *)
(* Environment management *)

(*
let add x y =
  match x, y with
  | Nb x, Nb y -> Nb (x+y)
  | _ -> LspUtils.type_error "expectd a number" (Cons(x, Cons (y, NIL)))
*)

let rec to_stack_list env = function
  | NIL -> []
  | Cons(Symb s,cdr) -> 
      (Array.unsafe_get env.values s.S.i) :: (to_stack_list env cdr)
  | _ -> assert false

let compile env x =
  let rec compile0 env x =
    match x with
    | TRUE 
    | Nb _
    | Str _
    | Port _
    | Env _ -> Quote x
    | NIL  -> x
    | Closure _  -> x
    | Subr _ -> x
    | Path (_,_) -> x
    | Symb _ -> x 
    | Unquote y -> Unquote (compile0 env y)
    | Quote y -> Quote (compile0 env y) 
    | Quasiquote y -> Quasiquote (compile0 env y)
    | Cons(Symb { S.name = "fun" ; _ }, 
           Cons(p,Cons(expr,NIL))) ->
        Closure(to_stack_list env p, [], compile0 env expr)
    | Cons(Symb { S.name = "quote" ; _ }, Cons(e, NIL)) ->
        Quote (compile0 env e)
    | Cons(Symb { S.name = "if" ; _ },
           Cons(c,Cons(t,Cons(e,NIL)))) ->
        If (compile0 env c, compile0 env t, compile0 env e)
    | Cons(Symb {S.name = "cond" ; _ }, args) -> 
        compile_cond env args
    | Cons(car, cdr) -> Cons (compile0 env car, compile0 env cdr)
    | _ -> assert false

  and compile_cond env = function
    | NIL -> NIL
    | Cons (Cons(TRUE,Cons(t,NIL)), _) -> compile0 env t
    | Cons(Cons(c,Cons(t,NIL)),e) -> 
        If(compile0 env c,compile0 env t,compile_cond env e)
    | x -> LspUtils.error2 "invalid expression" x
  in
  compile0 env x

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

and eval env x = 
  match x with
  | TRUE -> x
  | Nb _ -> x
  | Str _-> x
  | Port _ -> x
  | Env _ -> x
  | NIL -> x
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
  | _ -> assert false

and eval_path env = function
  | Symb x         -> full_lookup env x
  | Path(Symb x,y) -> eval_path (lookup env x) y
  | x              -> LspUtils.type_error "expected a path or a symbol" x

and eval_app_path env args = function
  | Symb s           -> app_eval env args (full_lookup env s)
  | Path (Symb p, s) -> eval_app_path (lookup env p) args s
  | x                -> app_eval env args x

and app_eval env args = function
  | Subr f -> 
      begin
        match f, args with
        | F1 f, Cons(x,NIL) -> f (eval env x)
        | F2 f, Cons(x, Cons(y,NIL)) -> f (eval env x) (eval env y)
        | Fn f, _ -> f env args
        | _, _ -> assert false
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
                  push p (eval env x);
                  let expr = eval env expr in
                  pop p;
                  expr
              | [p1;p2], Cons(x,Cons(y,NIL)) -> 
                  let nx = eval env x in
                  let ny = eval env y in
                  push p1 nx;
                  push p2 ny;
                  let expr = eval env expr in
                  pop p1;
                  pop p2;
                  expr
              | _                            ->
                  applyN env expr [] params args
            end
        | l -> 
            applyN env expr l params args
      end
  | x -> LspUtils.type_error "expected to be a 'fun'" x

and pushN l = Misc.iter (fun (s,e) -> push s e) l

and popN l = Misc.iter (fun (s, _) -> pop s) l 

and applyN env expr l params args =
  match params, args with
  | [], NIL -> 
      pushN l;
      let ne = eval env expr in
      popN l;
      ne
  | _ , NIL -> Closure(params, l, expr)
  | i :: y, Cons(a,b) ->
      applyN env expr ((i,eval env a) :: l) y b 
  | [], _ -> error "too many arguments" 
  | _,_ -> assert false

and add env acc = function
  | NIL -> Nb acc
(*  | Cons(x, Cons(y, NIL)) ->
      (match eval env x with
      | Nb x ->
          (match eval env y with
          | Nb y -> Nb (x + y + acc)
          | _ -> LspUtils.type_error "expected numbers" y)
      | _ -> LspUtils.type_error "expected numbers" x)*)
  | Cons(x,l) ->
      (match eval env x with
      | Nb n -> add env (n + acc) l
      | _ -> LspUtils.type_error "expected a number" x)
  | t -> LspUtils.error2 "incompatible argument" t

