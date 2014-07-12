open Term

(* ************************************************************************** *)
(* subr definition *)

(* Checks *)
let car = function Cons(c,_) -> c | t -> error2 "not a cons cell" t
let cdr = function Cons(_,c) -> c | t -> error2 "not a cons cell" t
let cadr t = car (cdr t)
let cddr t = cdr (cdr t)
let is_null = function NIL -> true | _ -> false
let is_binary t = is_null(cddr t)
let is_unary t = is_null(cdr t)
let is_ternary t = is_null(cdddr t)

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
    | NIL | TRUE | Subr _ | Closure _ 
    | Str _ | Nb _ | Port _ | Env _ -> y
    | Symb _ | Quote _ | Path (_,_) -> (*y*)
        if n>1 then Quote (quasiquote env (n-1) y) else y
    | Cons(x, l) -> Cons(quasiquote env n x, quasiquote env n l)
    | If(c,t,e) -> 
        If(quasiquote env n c, quasiquote env n t, quasiquote env n e)
    | Unquote z -> unquote env (n-1) z
    | Quasiquote y -> quasiquote env (n+1) y

and unquote env n y = 
  if n != 0 then quasiquote env n y else let _,e = eval_c env y in e


let rec eval_c env x = 
  try
    match x with
    | NIL | TRUE | Nb _ | Str _ | Port _ 
    | Env _ | Subr _ | Closure _ -> 
        env, x
    | Symb s -> env, lookup env s
    | Path (_,_) as x-> eval_path env x
    | If(c,t,e) ->
        begin
          match (eval_c env c) with
          | _, NIL -> eval_c env e
          | _,_ -> eval_c env t
        end
    | Quote y -> env, y
    | Quasiquote y -> env, quasiquote env 1 y
    | Cons(car, cdr) -> env, app_eval env car cdr
    | _ -> assert false
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
  | Path _ -> let ne, f = eval_path env f in app_eval ne f args 
  | Symb s -> app_eval env (lookup env s) args
  | Closure(params, l, expr) -> apply_c env expr l params args
  | _ -> Cons(f, args)

and apply_c env expr l params args =
  let rec f acc1 p a =
    match p, a with
    | [], NIL -> bind env expr acc1
    | [], _ -> error "too many arguments"
    | _, NIL -> Closure(p, acc1, expr)
    | (Symb x) :: y, Cons(a,b) ->
        let c = { value = snd (eval_c env a) ; plist = PList.empty } in
        f ((x.E.i,c)::acc1) y b 
    | _ -> assert false in
  f l params args

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
and do_quote _ t = car t

(* ************************************************************************** *)
(* PROPERTY LIST functions *)
and do_addprop env t =
  let x, y, z = eval env (car t), eval env (cadr t), eval env (caddr t) in
  addprop env (symb_of_cell x) (symb_of_cell y) z;
  z

and do_getprop env t =
  let x, y = eval env (car t), eval env (cadr t) in
  getprop env (symb_of_cell x) (symb_of_cell y) 

and do_remprop env t =
  let x, y = eval env (car t), eval env (cadr t) in
  remprop env (symb_of_cell x) (symb_of_cell y);
  x

and do_plist env t =  
  let _,x = eval_c env (car t) in
  let l = Term.PList.bindings (lookup_plist env (symb_of_cell x)) in
  Misc.fold 
    (fun (x,y) a -> Cons(Symb x, Cons(y,a))) 
    (Misc.reverse l) 
    NIL

and do_set_plist env t =
  let x,l = symb_of_cell (eval env (car t)), eval env (cadr t) in
  let rec loop l =
    if is_null l then ()
    else 
      begin
        addprop env x (symb_of_cell (car l)) (cadr l);
        loop (cddr l)
      end
  in
  rstprop env x;
  loop l;
  l
  
(* ************************************************************************** *)
(* BINDING functions *)
and def_rec x e env =
  match x with
  | Path(x,y) -> 
      let env' = 
        try (env_of_cell (eval env x))
        with Error -> 
          let env' = E.create !current_env (symb_of_cell x) in
          extend_global (symb_of_cell x) (Env env') env;
          env' in
      def_rec y e env' 
  | Symb nx -> extend_global nx e env
  | _ -> error2 "expected to be a path or a symbol" x 
  
(* 'let' definition : "(let (x v) expr)" *)
and  do_let env t =
  let rec f acc = function
    | Cons(e,NIL) -> bind env e acc 
    | Cons(Cons(Symb x,Cons(y,NIL)),tl) ->
        let c = {value = eval env y; plist = PList.empty} in
        f ((x.Env.i,c)::acc) tl 
    | _ -> error2 "incompatible argument" t in
  f [] t

(* 'setq' definition *)
and do_setq env t =
  match t with
  | Cons(label, Cons(e,NIL)) ->
      let _,expr = eval_c env e in
      def_rec label expr env;
      expr
  | _ -> error2 "incompatible argument" t

(* 'set' definition *)
and do_set env t =
  let label = eval env (car t) and expr = eval env (cadr t) in
  def_rec label expr env;
  expr

and do_define env t = 
  let label = car t and expr = eval env (cadr t) in
  def_rec label expr env;
  label

and do_defun env t =
  let label = car t and args = cadr t and body = caddr t in
  let expr = Closure(unbox args, [], body) in
  def_rec label expr env;
  label

(* ************************************************************************** *)
(* EQUALITY function *)

and cell_of_bool c = if c then TRUE else NIL

and do_equal env l =
  let o1, o2 = eval env (car l), eval env (cadr l) in
  if is_null (cddr l) then if o1=o2 then TRUE else NIL
  else error "'eq?' is given too many arguments"

and do_eq env = function
  | Cons(x,Cons(y,NIL)) ->
    (match eval_c env x, eval_c env y with
    | (_,NIL), (_,NIL) -> TRUE
    | (_,Nb s), (_,Nb t) -> if s==t then TRUE else NIL
    | (_,Symb s), (_,Symb t) -> if s==t then TRUE else NIL
    | (_,Str s), (_,Str t) -> if s==t then TRUE else NIL
    | (_,s),(_,t) -> if s==t then TRUE else NIL)
  | _ -> error ("'eq?' is a binary operator")


(* ************************************************************************** *)
(* COMPARISON Predicates *)
and do_gt env = function
  | Cons(x,Cons(y,NIL)) ->
      (match eval_c env x, eval_c env y with
      | (_,Nb x),(_,Nb y) -> if (x:int)>(y:int) then TRUE else NIL
      | _, _ -> error "Relational operator apply on num values") 
  | t -> error2 "Relational operators are binary operators" t

and do_gte env = function
  | Cons(x,Cons(y,NIL)) ->
      (match eval_c env x, eval_c env y with
      | (_,Nb x), (_,Nb y) -> if (x:int)>=(y:int) then TRUE else NIL
      | _, _ -> error "Relational operator apply on num values") 
  | t -> error2 "Relational operators are binary operators" t

and do_lt env = function
  | Cons(x,Cons(y,NIL)) ->
      (match eval_c env x, eval_c env y with
      | (_,Nb x), (_,Nb y) -> if (x:int)<(y:int) then TRUE else NIL
      | _, _ -> error "Relational operator apply on num values") 
  | t -> error2 "Relational operators are binary operators" t

and do_lte env = function
  | Cons(x,Cons(y,NIL)) ->
      (match eval_c env x, eval_c env y with
      | (_,Nb x), (_,Nb y) -> if (x:int)<=(y:int) then TRUE else NIL
      | _, _ -> error "Relational operator apply on num values") 
  | t -> error2 "Relational operators are binary operators" t

(* ************************************************************************** *)
(* BASIC "typing" Predicates *)

and do_nump env t =
  if is_unary t then cell_of_bool (is_num (eval env (car t)))
  else error "'num?' is given too many arguments"

and do_consp env t =
  if is_unary t then cell_of_bool (is_cons (eval env (car t)))
  else error "'cons?' is given too many arguments"
and do_stringp env t =
  if is_unary t then cell_of_bool (is_string (eval env (car t)))
  else error "'string?' is given too many arguments"
and do_symbp env t =
  if is_unary t then cell_of_bool (is_symb (eval env (car t)))
  else error "'symb?' is given too many arguments"
and do_nullp env = function
  | Cons(x,NIL) -> 
      (match (eval_c env x) with
      | _, NIL -> TRUE
      | _ -> NIL)
  | t -> error2 "'null?' unary function" t
and do_envp env = function
  | Cons(x,NIL) -> cell_of_bool (is_env (eval env x))
  | t -> error2 "too many arguments" t

(* ************************************************************************** *)
(* LISTS function *)

(* CONS, CAR, CDR subr *)
and do_cons env t =
  match t with
  | Cons(x,Cons(y,NIL)) -> Cons(eval env x,eval env y)
  | _ -> error2 "'cons' - incompatible arguments" t
and do_car env = function
  | Cons(x, _) -> 
      (match eval_c env x with
      | _, Cons(a,_) -> a
      | _, NIL -> NIL
      | _, y -> error2 "not a cons cell" y)
  | _ -> error "unary operator"
and do_cdr env = function
  | Cons(x, _) -> 
      (match eval_c env x with
      | _, Cons(_,a) -> a
      | _, NIL -> NIL
      | _, y -> error2 "not a cons cell" y)
  | _ -> error "unary operator"

and do_list env t =
  let rec cons acc t =
    if is_null t then Misc.fold (fun t a -> mk_cons t a) acc NIL
    else cons ((eval env (car t))::acc) (cdr t) in
  cons [] t

and do_nth env t =
  let rec find n l =
    if is_null l then error "out of bound access"
    else 
      if n=0 then car l else find (n-1) (cdr l) 
  in find (int_of_cell (eval env (car t))) (eval env (cadr t))

and do_map env t =
  let f = eval env (car t) and l = eval env (cadr t) in
  let rec map0 f l =
    if is_null l then NIL
    else Cons(eval env (Cons(f,Cons(car l,NIL))),map0 f (cdr l))
  in map0 f l

(* ************************************************************************** *)
(* ARITHMETIC *)

and add env acc = function
  | NIL -> acc
  | Cons(x,l) -> 
      (match eval_c env x with
      | _, Nb n -> add env (n + acc) l
      | _, _ -> error2 "expected a number" x)
  | t -> error2 "incompatible argument" t

and do_plus env t = Nb (add env 0 t)

and do_succ env = function
  | Cons(x,NIL) ->
      (match eval_c env x with
      | _, Nb n -> Nb(succ n)
      | _, _ -> error2 "num type expected" x)
  | t -> error2 "'succ' arity error" t

and do_pred env = function
  | Cons(x,NIL) ->
      (match eval_c env x with
      | _, Nb n -> Nb(pred n)
      | _, _ -> error2 "num type expected" x)
  | t -> error2 "'pred' arity error" t

and do_minus env t =
    match t with
    | Cons(x,l) ->
        (match eval_c env x with
        | _, Nb n -> Nb(n-(add env 0 l))
        | _, _ -> error2 "expected a number" x)
    | _ -> error2 "expected at least 2 args" t 

and do_mult env t = 
  let rec mult t acc =
    if is_null t then acc
    else mult (cdr t) ((int_of_cell (eval env (car t))) * acc)
  in Nb (mult t 1)

and do_div env t =
  let den = int_of_cell (do_mult env (cdr t)) in
  if den==0 then 
    error "div by zero" 
  else Nb ((int_of_cell (eval env (car t)))/den)

(*
and do_mod t = 
  let o1, o2 = car t, cadr t in
  if is_binary t then Nb ((int_of_cell o1) mod (int_of_cell))
  else error "'mod' is given too many arguments"
*)

(* ************************************************************************** *)
(* BOOLEAN *)

and do_and env t = 
  let rec doand = function
    | NIL -> TRUE
    | Cons(x,y) ->
        (match eval_c env x with
        | _,NIL -> NIL 
        | _ -> doand y)
    | t -> error2 "unexpected argument" t 
  in doand t

and do_or env t =
  let rec door = function
    | NIL -> NIL
    | Cons(x,y) ->
        (match eval_c env x with
        | _, NIL -> door y 
        | _,nx -> nx)
    | t -> error2 "unexpected argument" t 
  in door t

and do_not env = function
  | Cons(x,NIL) -> if (eval env x) == NIL then TRUE else NIL
  | t -> error2 "unexpected argument" t

(* ************************************************************************** *)
(* STRINGS *)
and do_cat env t =
  let rec docat t acc =
    if is_null t then acc
    else docat (cdr t) (acc^(string_of_cell (eval env (car t)))) in
  Str (docat t "")


(* ************************************************************************** *)
(* Imperative-style SEQUENCING *)

and do_seq env t =
  let rec f t =
    if (is_null (cdr t)) then eval env (car t)
    else 
      begin
        ignore (eval env (car t));
        f (cdr t)
      end 
  in f t


(* ************************************************************************** *)
(* While Loop *)
and iterate env c e acc =
  match (eval_c env c) with
  | _, NIL -> acc
  | _, _ -> iterate env c e (eval env e)

and do_while env t =
  match t with
  | Cons(c, Cons (e, NIL)) ->
      iterate env c e NIL
  | _ -> error2 "expected 2 arguments" t

(* ************************************************************************** *)
(* IO *)

and do_println env t =
  let rec f t acc = 
    if is_null (cdr t) then 
      begin
        let s = eval env (car t) in
        print_endline (acc^(pp' s));
        s
      end
    else f (cdr t) (acc^(pp' (eval env (car t)))) in
  f t ""

and do_print env t =
   let rec f t acc = 
    if is_null (cdr t) then 
      begin
        let s = eval env (car t) in
        print_string (acc^(pp' s)); flush stdout;
        s
      end
    else f (cdr t) (acc^(pp' (eval env (car t)))) in
  f t ""

and do_write env t =
  let p = eval env (car t) in
  let s = eval env (cadr t) in
  if is_output_port p then
    if is_string s then
      begin
        let _,b,c = get_output_port p in
        let s' = string_of_cell s in
        (match (b,c) with
        | Some buf , None -> Buffer.add_string buf s'
        | None, Some c -> output_string c s'
        | _, _ -> assert false);
        TRUE
      end
    else error2 "expected to be a string" s
  else error2 "expected to be an output-port" p

and do_open_input_file env t =
  let fn = string_of_cell (eval env (car t)) in
  let p = 
    let c = open_in fn in
    let lb = Lexing.from_channel c in
    Port(Input(fn,lb,Some c)) in
  p

and do_open_input_string env t =
  let s = string_of_cell (eval env (car t)) in
  let lb = Lexing.from_string s in
  Port(Input("",lb,None))

and do_close_input_file env t =
  let p = eval env (car t) in
  if is_input_file p then
    begin
      let _,_, c = get_input_port p in
      close_in (Misc.get_opt c);
      TRUE;
    end
  else error2 "expected to be an input_port" (car t)

and do_open_output_file env t =
  let fn = string_of_cell (eval env (car t)) in
  let p = 
    let c = open_out fn in
    Port(Output(fn,None,Some c)) in
  p

and do_open_output_string env t =
  let s = string_of_cell (eval env (car t)) in
  let buf = Buffer.create 80 in
  Buffer.add_string buf s;
  Port(Output("",Some buf,None))

and do_flush_output_string env t =
  let p = eval env (car t) in
  if is_output_string p then
    begin
      let _,b,_ = get_output_port p in
      Buffer.reset (Misc.get_opt b);
      TRUE
    end
  else error2 "expected to be an output-string" (car t)

and do_get_output_string env t =
  let p = eval env (car t) in
  if is_output_string p then
    begin
      let _,b,_ = get_output_port p in
      Str(Buffer.contents (Misc.get_opt b))
    end
  else error2 "expected to be an output-string" (car t)

and do_close_output_file env t =
  let p = eval env (car t) in
  if is_output_file p then
    begin
      let _,_,c = get_output_port p in
      close_out (Misc.get_opt c);
      TRUE
    end
  else error2 "expected to be an input_port" (car t)

and do_read env t = 
    if is_null t then Lexer.build_object !Globals.current_channel
    else 
      let _,lb,_ = 
        let p = eval env (car t) in
        if is_input_port p then get_input_port p 
        else error2 "expected to be an input port" p 
      in Lexer.build_object_lex lb

and do_read_line env t =
  let _,_,c = 
    let p = eval env (car t) in
    if is_input_port p then get_input_port p 
    else error2 "expected to be an input port" p in
  Str(input_line (Misc.get_opt c))

(* ************************************************************************** *)
(* ENVIRONMENT *)

and do_make_env env t =
  let e = eval env (car t) in
  if is_symb e then 
    begin
      let env' = Env (E.create !current_env (symb_of_cell e)) in
      extend_global (symb_of_cell e) env' env;
      current_env := env_of_cell env';
      e
    end
  else 
    if is_env e then 
      begin
        current_env := (env_of_cell e); 
        car t
      end
    else error2 "expected to be a symbol or environment expression" (car t)

and do_in_env env t =
  let e = eval env (car t) in
  let env' = 
    if is_env e then env_of_cell e else error2 "expected to be an env" e in
  current_env := env';
  TRUE

and do_symbols env t =
  let f l = Misc.fold (fun k l' -> Cons(Symb k, l')) (Misc.reverse l) NIL in
  let e = eval env (car t) in
  let env' = 
    if is_env e then env_of_cell e
    else error2 "expected to be an environment" e 
  in f (Env.symbols env')

(* ************************************************************************** *)
(* EVALUATION *)
and do_eval env t =
  let _, res =
    match t with
    | Cons(Quote x,_) -> eval_c env x
    | Cons(x,_) -> let _, y = eval_c env x in eval_c env y
    | _ -> error2 "invalid argument" t in
  res

(* ************************************************************************** *)
(* EXECUTION TIME *)
and do_time env t =
  let t1 = Sys.time() in
  let r = eval env (car t) in
  let t2 = Sys.time () in
  print_endline ((string_of_float (t2 -. t1))^" sec");
  r

(* ************************************************************************** *)
(* Shell Command *)
and do_exec env t =
  Nb (Sys.command (string_of_cell (eval env (car t))))
  
(* ************************************************************************** *)
(* EXIT *)
and do_exit _ _ = raise End_of_file

(* ************************************************************************** *)
(* PERVASIVES *)
let add_subr s f = 
  extend_global (Env.symbol !current_env s) (Subr f) !current_env

let intern s =
  extend_global (Env.symbol !current_env s) NIL !current_env

let init_global () =
  extend_global 
    (Env.symbol !current_env "MAIN") 
    (Env !current_env)
    !current_env;
  add_subr "in-package" do_make_env;
  add_subr "symbols" do_symbols;
  add_subr "define" do_define;
  add_subr "defun" do_defun;
  add_subr "quote" do_quote;
  add_subr "eval" do_eval;
  add_subr "set" do_set;
  add_subr "setq" do_setq;
  add_subr "let" do_let;
  add_subr "eq?" do_eq;
  add_subr "equal?" do_equal;
  add_subr "num?" do_nump;
  add_subr "cons" do_cons;
  add_subr "car" do_car;
  add_subr "cdr" do_cdr;
  add_subr "nth" do_nth;
  add_subr "list" do_list;
  add_subr "map" do_map;
  add_subr "null?" do_nullp;
  add_subr "+" do_plus;
  add_subr "-" do_minus;
  add_subr "*" do_mult;
  add_subr "/" do_div;
  add_subr ">" do_gt;
  add_subr "<" do_lt;
  add_subr ">=" do_gte;
  add_subr "<=" do_lte;
  add_subr "and" do_and;
  add_subr "or" do_or;
  add_subr "not" do_not;
  add_subr "cat" do_cat;
  add_subr "do" do_seq;
  add_subr "while" do_while;
  add_subr "exit" do_exit;
  add_subr "time" do_time;
  add_subr "print" do_print;
  add_subr "println" do_println;
  add_subr "write" do_write;
  add_subr "exec" do_exec;
  add_subr "read" do_read;
  add_subr "open-input-file" do_open_input_file;
  add_subr "open-input-string" do_open_input_string;
  add_subr "close-input-file" do_close_input_file;
  add_subr "close-input-string" do_close_input_file;
  add_subr "open-output-file" do_open_output_file;
  add_subr "open-output-string" do_open_output_string;
  add_subr "flush-output-string" do_flush_output_string;
  add_subr "get-output-string" do_get_output_string;
  add_subr "close-output-file" do_close_output_file;
  add_subr "read-line" do_read_line;
  add_subr "add-prop" do_addprop;
  add_subr "get-prop" do_getprop;
  add_subr "rem-prop" do_remprop;
  add_subr "get-plist" do_plist;
  add_subr "set-plist" do_set_plist;
  add_subr "succ" do_succ;
  add_subr "pred" do_pred;;
