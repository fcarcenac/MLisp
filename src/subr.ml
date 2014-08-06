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
  if n != 0 then quasiquote env n y else snd (eval_c env y)

and eval_c env x =
    match x with
    | NIL | TRUE | Nb _ | Str _ | Port _ 
    | Env _ | Subr _ | Closure _ -> env, x
    | Symb s -> env, full_lookup env s
    | Path(_,_) as x -> eval_path env x
    | If(c,t,e) ->
        if (snd (eval_c env c)) = TRUE then eval_c env t else eval_c env e
    | Quote y -> env, y
    | Quasiquote y -> env, quasiquote env 1 y
    | Cons(car, cdr) -> env, app_eval env car cdr
    | _ -> assert false

and eval_path env = function
  | Symb x -> env, lookup env x
  | Path(Symb x,y) -> 
      eval_path (env_of_cell (lookup env x)) y
  | _ -> assert false

and app_eval env f args =
  match f with
  | Symb s -> app_eval env (full_lookup env s) args
  | Subr (F1 cf) -> cf (snd (eval_c env (car args))) 
  | Subr (F2 cf) -> 
      cf (snd (eval_c env (car args))) (snd (eval_c env (cadr args)))
  | Subr (Fn cf) -> cf env args
  | Closure(params, l, expr) -> apply_c env expr l params args
  | Path _ -> let ne, f = eval_path env f in app_eval ne f args 
  | _ -> Cons(f, args)

and apply_c env expr l params args =
  let rec f acc1 p a =
    match p, a with
    | _, NIL -> 
        if p = [] 
        then bind env expr acc1 
        else Closure(p, acc1, expr)
    | (Symb x) :: y, Cons(a,b) ->
        let c = { value = snd (eval_c env a) ; plist = PList.empty } in
        f ((x.E.i,c)::acc1) y b 
    | _ -> 
        if p = [] then error "too many arguments" else assert false 
  in
  f l params args

and bind env exp l =
  let rec f l acc =
    match l with
    | [] -> (*snd (eval_c env exp)*) apply env exp acc
    | (id,c)::tl ->
        let v = E.O.A.get env.E.values id in
        E.O.A.set env.E.values id (c :: v);
        f tl ((id,v)::acc)
  in
  f l []

and apply env exp l = pop_list env (snd (eval_c env exp)) l


and pop_list env e = function
  | [] -> e
  | (id,v)::tl -> 
      E.O.A.set env.E.values id v;
      pop_list env e tl

and eval env x = snd (eval_c env x)

let do_quote t = car t

(* ************************************************************************** *)
(* PROPERTY LIST functions *)
let do_addprop env t =
  let x, y, z = eval env (car t), eval env (cadr t), eval env (caddr t) in
  addprop env (symb_of_cell x) (symb_of_cell y) z;
  z

let do_getprop env t =
  let x, y = eval env (car t), eval env (cadr t) in
  getprop env (symb_of_cell x) (symb_of_cell y) 

let do_remprop env t =
  let x, y = eval env (car t), eval env (cadr t) in
  remprop env (symb_of_cell x) (symb_of_cell y);
  x

let do_plist env t =  
  let _,x = eval_c env (car t) in
  let l = Term.PList.bindings (lookup_plist env (symb_of_cell x)) in
  Misc.fold 
    (fun (x,y) a -> Cons(Symb x, Cons(y,a))) 
    (Misc.reverse l) 
    NIL

let do_set_plist env t =
  let x,l = symb_of_cell (eval env (car t)), eval env (cadr t) in
  let rec loop l =
    match l with 
    | NIL -> ()
    | _   -> 
        addprop env x (symb_of_cell (car l)) (cadr l);
        loop (cddr l)
  in
  rstprop env x;
  loop l;
  l
  
(* ************************************************************************** *)
(* BINDING functions *)
let rec def_rec x e env =
  match x with
  | Path(Symb s,y) -> 
      let env' = 
        try 
          print_endline "@@";
          (env_of_cell (eval env x))
        with Error -> 
          let env' = E.e_child !current_env 769 769 s in
          extend_global s (Env env') env;
          env' in
      def_rec y e env' 
  | Symb nx -> extend_global nx e env
  | _ -> error2 "expected to be a path or a symbol" x 
  
(* 'let' definition : "(let (x v) expr)" *)
let  do_let env t =
  let rec f acc = function
    | Cons(e,NIL) -> bind env e acc 
    | Cons(Cons(Symb x,Cons(y,NIL)),tl) ->
        let c = {value = eval env y; plist = PList.empty} in
        f ((x.Env.i,c)::acc) tl 
    | _ -> error2 "incompatible argument" t in
  f [] t

(* 'setq' definition *)
let do_setq env t =
  match t with
  | Cons(label, Cons(e,NIL)) ->
      let _,expr = eval_c env e in
      def_rec label expr env;
      expr
  | _ -> error2 "incompatible argument" t

(* 'set' definition *)
let do_set env t =
  let label = eval env (car t) and expr = eval env (cadr t) in
  def_rec label expr env;
  expr

let do_define env t = 
  let label = car t and expr = eval env (cadr t) in
  def_rec label expr env;
  label

let do_defun env t =
  let label = car t and args = cadr t and body = caddr t in
  let expr = Closure(unbox args, [], body) in
  def_rec label expr env;
  label

(* ************************************************************************** *)
(* EQUALITY function *)

let cell_of_bool c = if c then TRUE else NIL

let do_equal o1 o2 = if o1=o2 then TRUE else NIL

let do_eq a b = 
  match a, b with
  | NIL, NIL -> TRUE
  | Nb s, Nb t -> if s==t then TRUE else NIL
  | Symb s, Symb t -> if s==t then TRUE else NIL
  | Str s, Str t -> if s==t then TRUE else NIL
  | _ -> if a==b then TRUE else NIL


(* ************************************************************************** *)
(* COMPARISON Predicates *)
let do_gt a b = 
  match a, b with
  | Nb x,Nb y -> if (x:int)<=(y:int) then NIL else TRUE
  | _, _ -> error "Relational operator apply on num values"

let do_gte a b =
  match a, b with
  | Nb x, Nb y -> if (x:int)>=(y:int) then TRUE else NIL
  | _, _ -> error "Relational operator apply on num values"

let do_lt a b = 
  match a, b with
  | Nb x, Nb y -> if (x:int)>=(y:int) then NIL else TRUE
  | _, _ -> error "Relational operator apply on num values"

let do_lte a b =
  match a, b with
  | Nb x, Nb y -> if (x:int)<=(y:int) then TRUE else NIL
  | _, _ -> error "Relational operator apply on num values" 

(* ************************************************************************** *)
(* BASIC "typing" Predicates *)

let do_nump t = cell_of_bool (is_num t)
let do_consp t = cell_of_bool (is_cons t)
let do_stringp t = cell_of_bool (is_string t)
let do_symbp t = cell_of_bool (is_symb t)
let do_nullp = function | NIL -> TRUE | _ -> NIL
let do_envp x = cell_of_bool (is_env x)

(* ************************************************************************** *)
(* LISTS function *)

(* CONS, CAR, CDR subr *)
let do_cons x y = Cons(x,y)
let do_car = car
let do_cdr = cdr

let do_list env t =
  let rec cons acc t =
    if is_null t then Misc.fold (fun t a -> mk_cons t a) acc NIL
    else cons ((eval env (car t))::acc) (cdr t) in
  cons [] t

let do_nth env t =
  let rec find n l =
    if is_null l then error "out of bound access"
    else 
      if n=0 then car l else find (n-1) (cdr l) 
  in find (int_of_cell (eval env (car t))) (eval env (cadr t))

let do_map env t =
  let f = eval env (car t) and l = eval env (cadr t) in
  let rec map0 f l =
    match l with
    | NIL -> NIL
    | Cons(car,cdr) -> Cons(eval env (Cons(f,Cons(car,NIL))),map0 f cdr)
    | _ -> assert false
  in map0 f l

(* ************************************************************************** *)
(* ARITHMETIC *)

let rec add env acc = function
  | NIL -> acc
  | Cons(x,l) -> 
      (match eval_c env x with
      | _, Nb n -> add env (n + acc) l
      | _, _ -> error2 "expected a number" x)
  | t -> error2 "incompatible argument" t

let do_plus env t = Nb (add env 0 t)

let do_succ = function
  | Nb n -> Nb(succ n)
  | x -> error2 "num type expected" x

let do_pred = function
  | Nb n -> Nb(pred n)
  | x -> error2 "num type expected" x

let do_minus env t =
    match t with
    | Cons(x,l) ->
        (match eval_c env x with
        | _, Nb n -> Nb(n-(add env 0 l))
        | _, _ -> error2 "expected a number" x)
    | _ -> error2 "expected at least 2 args" t 

let do_mult env t = 
  let rec mult t acc =
    if is_null t then acc
    else mult (cdr t) ((int_of_cell (eval env (car t))) * acc)
  in Nb (mult t 1)

let do_div env t =
  let den = int_of_cell (do_mult env (cdr t)) in
  if den==0 then 
    error "div by zero" 
  else Nb ((int_of_cell (eval env (car t)))/den)

(*
let do_mod t = 
  let o1, o2 = car t, cadr t in
  if is_binary t then Nb ((int_of_cell o1) mod (int_of_cell))
  else error "'mod' is given too many arguments"
*)

(* ************************************************************************** *)
(* BOOLEAN *)

let do_and env t = 
  let rec doand = function
    | NIL -> TRUE
    | Cons(x,y) ->
        (match eval_c env x with
        | _,NIL -> NIL 
        | _ -> doand y)
    | t -> error2 "unexpected argument" t 
  in doand t

let do_or env t =
  let rec door = function
    | NIL -> NIL
    | Cons(x,y) ->
        (match eval_c env x with
        | _, NIL -> door y 
        | _,nx -> nx)
    | t -> error2 "unexpected argument" t 
  in door t

let do_not env = function
  | Cons(x,NIL) -> if (eval env x) == NIL then TRUE else NIL
  | t -> error2 "unexpected argument" t

(* ************************************************************************** *)
(* STRINGS *)
let do_cat env t =
  let rec docat t acc =
    if is_null t then acc
    else docat (cdr t) (acc^(string_of_cell (eval env (car t)))) in
  Str (docat t "")


(* ************************************************************************** *)
(* Imperative-style SEQUENCING *)

let do_seq env t =
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
let rec iterate env c e acc =
  match (eval_c env c) with
  | _, NIL -> acc
  | _, _ -> iterate env c e (eval env e)

let do_while env t =
  match t with
  | Cons(c, Cons (e, NIL)) ->
      iterate env c e NIL
  | _ -> error2 "expected 2 arguments" t

(* ************************************************************************** *)
(* IO *)

let do_println env t =
  let rec f t acc = 
    if is_null (cdr t) then 
      begin
        let s = eval env (car t) in
        print_endline (acc^(pp' s));
        s
      end
    else f (cdr t) (acc^(pp' (eval env (car t)))) in
  f t ""

let do_print env t =
   let rec f t acc = 
    if is_null (cdr t) then 
      begin
        let s = eval env (car t) in
        print_string (acc^(pp' s)); flush stdout;
        s
      end
    else f (cdr t) (acc^(pp' (eval env (car t)))) in
  f t ""

let do_write env t =
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

let do_open_input_file env t =
  let fn = string_of_cell (eval env (car t)) in
  let p = 
    let c = open_in fn in
    let lb = Lexing.from_channel c in
    Port(Input(fn,lb,Some c)) in
  p

let do_open_input_string env t =
  let s = string_of_cell (eval env (car t)) in
  let lb = Lexing.from_string s in
  Port(Input("",lb,None))

let do_close_input_file env t =
  let p = eval env (car t) in
  if is_input_file p then
    begin
      let _,_, c = get_input_port p in
      close_in (Misc.get_opt c);
      TRUE;
    end
  else error2 "expected to be an input_port" (car t)

let do_open_output_file env t =
  let fn = string_of_cell (eval env (car t)) in
  let p = 
    let c = open_out fn in
    Port(Output(fn,None,Some c)) in
  p

let do_open_output_string env t =
  let s = string_of_cell (eval env (car t)) in
  let buf = Buffer.create 80 in
  Buffer.add_string buf s;
  Port(Output("",Some buf,None))

let do_flush_output_string env t =
  let p = eval env (car t) in
  if is_output_string p then
    begin
      let _,b,_ = get_output_port p in
      Buffer.reset (Misc.get_opt b);
      TRUE
    end
  else error2 "expected to be an output-string" (car t)

let do_get_output_string env t =
  let p = eval env (car t) in
  if is_output_string p then
    begin
      let _,b,_ = get_output_port p in
      Str(Buffer.contents (Misc.get_opt b))
    end
  else error2 "expected to be an output-string" (car t)

let do_close_output_file env t =
  let p = eval env (car t) in
  if is_output_file p then
    begin
      let _,_,c = get_output_port p in
      close_out (Misc.get_opt c);
      TRUE
    end
  else error2 "expected to be an input_port" (car t)

let do_read env t = 
    if is_null t then Lexer.build_object !Globals.current_channel
    else 
      let _,lb,_ = 
        let p = eval env (car t) in
        if is_input_port p then get_input_port p 
        else error2 "expected to be an input port" p 
      in Lexer.build_object_lex lb

let do_read_line env t =
  let _,_,c = 
    let p = eval env (car t) in
    if is_input_port p then get_input_port p 
    else error2 "expected to be an input port" p in
  Str(input_line (Misc.get_opt c))

(* ************************************************************************** *)
(* ENVIRONMENT *)

let do_make_env env t =
  let e = eval env (car t) in
  if is_symb e then 
    begin
      let env' = Env (E.e_child !current_env 769 769 (symb_of_cell e)) in
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

let do_symbols t =
  let f l = Misc.fold (fun k l' -> Cons(Symb k, l')) (Misc.reverse l) NIL in
  f (Env.symbols (env_of_cell t))

(* ************************************************************************** *)
(* EVALUATION *)
let do_eval env t =
  let _, res =
    match t with
    | Cons(Quote x,_) -> eval_c env x
    | Cons(x,_) -> let _, y = eval_c env x in eval_c env y
    | _ -> error2 "invalid argument" t in
  res

(* ************************************************************************** *)
(* EXECUTION TIME *)
let do_time env t =
  let t1 = Sys.time() in
  let r = eval env (car t) in
  let t2 = Sys.time () in
  print_endline ((string_of_float (t2 -. t1))^" sec");
  r

(* ************************************************************************** *)
(* Shell Command *)
let do_exec env t =
  Nb (Sys.command (string_of_cell (eval env (car t))))
  
(* ************************************************************************** *)
(* EXIT *)
let do_exit _ _ = raise End_of_file

(* ************************************************************************** *)
(* PERVASIVES *)
let add_subr s f = 
  extend_global (Env.symbol !current_env s) (Subr (Fn f)) !current_env

let add_subr1 s f = 
  extend_global (Env.symbol !current_env s) (Subr (F1 f)) !current_env

let add_subr2 s f = 
  extend_global (Env.symbol !current_env s) (Subr (F2 f)) !current_env

let intern s =
  extend_global (Env.symbol !current_env s) NIL !current_env

let init_global () =
  extend_global 
    (Env.symbol !current_env "MAIN") 
    (Env !current_env)
    !current_env;
  add_subr "in-package" do_make_env;
  add_subr1 "symbols" do_symbols;
  add_subr "define" do_define;
  add_subr "defun" do_defun;
  add_subr1 "quote" do_quote;
  add_subr "eval" do_eval;
  add_subr "set" do_set;
  add_subr "setq" do_setq;
  add_subr "let" do_let;
  add_subr2 "eq?" do_eq;
  add_subr2 "equal?" do_equal;
  add_subr1 "num?" do_nump;
  add_subr2 "cons" do_cons;
  add_subr1 "car" do_car;
  add_subr1 "cdr" do_cdr;
  add_subr "nth" do_nth;
  add_subr "list" do_list;
  add_subr "map" do_map;
  add_subr1 "null?" do_nullp;
  add_subr "+" do_plus;
  add_subr "-" do_minus;
  add_subr "*" do_mult;
  add_subr "/" do_div;
  add_subr2 ">" do_gt;
  add_subr2 "<" do_lt;
  add_subr2 ">=" do_gte;
  add_subr2 "<=" do_lte;
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
  add_subr1 "succ" do_succ;
  add_subr1 "pred" do_pred;;
