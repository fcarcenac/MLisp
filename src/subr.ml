open Lsp
open Eval

module S = Symbol

(* ************************************************************************** *)
(* subr definition *)

(* Checks *)
let car = function Cons(c,_) -> c | t -> LspUtils.error2 "not a cons cell" t
let cdr = function Cons(_,c) -> c | t -> LspUtils.error2 "not a cons cell" t
let cadr t = car (cdr t)
let cddr t = cdr (cdr t)
let caddr t = car (cdr (cdr t))
(*let cdddr t = cdr (cdr (cdr t))*)
let is_null = function NIL -> true | _ -> false
(*
let is_binary t = is_null(cddr t)
let is_unary t = is_null(cdr t)
let is_ternary t = is_null(cdddr t)

let do_quote t = car t*)

(* ************************************************************************** *)
(* PROPERTY LIST functions *)
let do_addprop env t =
  let x, y, z = eval env (car t), eval env (cadr t), eval env (caddr t) in
  LspUtils.addprop 
    env 
    (LspUtils.symb_of_cell x) 
    (LspUtils.symb_of_cell y) 
    z;
  z

let do_getprop env t =
  let x, y = eval env (car t), eval env (cadr t) in
  LspUtils.getprop 
    env 
    (LspUtils.symb_of_cell x) 
    (LspUtils.symb_of_cell y) 

let do_remprop env t =
  let x, y = eval env (car t), eval env (cadr t) in
  LspUtils.remprop 
    env 
    (LspUtils.symb_of_cell x) 
    (LspUtils.symb_of_cell y);
  x

let do_plist env t =  
  let x = eval env (car t) in
  let l = 
    PropList.bindings 
      (LspUtils.lookup_plist env (LspUtils.symb_of_cell x)) in
  Misc.fold 
    (fun (x,y) a -> Cons(Symb x, Cons(y,a))) 
    (Misc.reverse l) 
    NIL

let do_set_plist env t =
  let x, l = LspUtils.symb_of_cell (eval env (car t)), eval env (cadr t) in
  let rec loop = function
    | NIL -> ()
    | l   -> 
        LspUtils.addprop env x (LspUtils.symb_of_cell (car l)) (cadr l);
        loop (cddr l)
  in
  LspUtils.rstprop env x;
  loop l;
  l
  
(* ************************************************************************** *)
(* BINDING functions *)
let rec def_rec x e env =
  match x with
  | Path(Symb s,y) -> 
      let env' = 
        try 
          LspUtils.env_of_cell (eval env x)
        with LspUtils.Error -> 

          let env' = LspUtils.e_child !Globals.init_env 769 s in
          LspUtils.extend_global s (Env env') env;
          env' in
      def_rec y e env' 
  | Symb nx -> LspUtils.extend_global nx e env
  | _ -> LspUtils.type_error "expected to be a path or a symbol" x 
  
(* 'let' definition : "(let (x v) expr)" *)
let  do_let env t =
  let rec f acc = function
    | Cons(e,NIL) -> 
        let e = eval env e in 
        popN acc;
        e
    | Cons(Cons(Symb x,Cons(y,NIL)),tl) ->
        (*TODO: remove
        let c = {value = NIL ; plist = PropList.empty } in*)
        let ny = eval env y in
        let s = Array.unsafe_get env.values x.S.i in
        push s ny;
        f ((s, ny) :: acc) tl 
    | _ -> LspUtils.error2 "incompatible argument" t in
  f [] t

(* 'setq' definition *)
let do_setq env t =
  match t with
  | Cons(label, Cons(e,NIL)) ->
      let expr = eval env e in
      def_rec label expr env;
      expr
  | _ -> LspUtils.error2 "incompatible argument" t

(* 'set' definition *)
let do_set env t =
  let expr = eval env (cadr t) in
  def_rec (eval env (car t)) expr env;
  expr

let do_define env t = 
  let label = car t in
  def_rec label (eval env (cadr t)) env;
  label

let do_defun env t =
  let label = car t and args = cadr t and body = caddr t in
  let expr = Closure(to_stack_list env args, [], body) in
  def_rec label expr env;
  label

(* ************************************************************************** *)
(* EQUALITY function *)

let cell_of_bool c = if c then TRUE else NIL

let do_equal (o1:cell) o2 = if o1=o2 then TRUE else NIL

let do_eq a b = 
  match a, b with
  | NIL, NIL -> TRUE
  | Nb s, Nb t -> if s==t then TRUE else NIL
  | Symb s, Symb t -> if s==t then TRUE else NIL
  | Str s, Str t -> if s==t then TRUE else NIL
  | _ -> if a==b then TRUE else NIL


(* ************************************************************************** *)
(* COMPARISON Predicates *)
let lte (x:int) (y:int) = x<=y
let gte (x:int) (y:int) = x>=y

let do_gt a b =
  match a, b with
  | Nb x, Nb y when lte x y -> NIL 
  | Nb _, Nb _              -> TRUE
      (*if lte x y then NIL else TRUE*)
  | _, _ -> error "Relational operator apply on num values"

let do_gte a b =
  match a, b with
  | Nb x, Nb y when gte x y -> TRUE 
  | Nb _, Nb _              -> NIL 
  | _, _ -> error "Relational operator apply on num values"

let do_lt a b = 
  match a, b with
  | Nb x, Nb y when gte x y -> NIL
  | Nb _, Nb _              -> TRUE
  | _, _ -> error "Relational operator apply on num values"

let do_lte a b =
  match a, b with
  | Nb x, Nb y when lte x y -> TRUE      
  | Nb _, Nb _              -> NIL      
  | _, _ -> error "Relational operator apply on num values" 

(* ************************************************************************** *)
(* BASIC "typing" Predicates *)

let do_nullp = function | NIL -> TRUE | _ -> NIL
let do_nump t = cell_of_bool (is_num t)
(*let do_consp t = cell_of_bool (is_cons t)
let do_stringp t = cell_of_bool (is_string t)
let do_symbp t = cell_of_bool (is_symb t)
let do_envp x = cell_of_bool (is_env x)*)

(* ************************************************************************** *)
(* LISTS function *)

(* CONS, CAR, CDR subr *)
let do_cons x y = Cons(x,y)

let do_list env t =
  let rec cons acc t =
    if is_null t then Misc.fold (fun t a -> Cons (t, a)) acc NIL
    else cons ((eval env (car t))::acc) (cdr t) in
  cons [] t

let do_nth env t =
  let rec find n l =
    if is_null l then error "out of bound access"
    else 
      if n=0 then car l else find (n-1) (cdr l) 
  in find (LspUtils.int_of_cell (eval env (car t))) (eval env (cadr t))

let do_map env t =
  let rec map0 f l =
    match l with
    | NIL -> NIL
    | Cons(car,cdr) -> Cons(eval env (Cons(f,Cons(car,NIL))),map0 f cdr)
    | _ -> LspUtils.type_error "list expected" l
  in map0 (eval env (car t)) (eval env (cadr t))

(* ************************************************************************** *)
(* ARITHMETIC *)
(*
let rec add env acc = function
  | Cons(x, Cons(y, NIL)) as args ->
      (match eval env x, eval env y with
      | Nb x, Nb y -> x + (y + acc)
      | _ -> LspUtils.type_error "expected numbers" args)
  | NIL -> acc
  | Cons(x,l) -> 
      (match eval env x with
      | Nb n -> add env (n + acc) l
      | _ -> LspUtils.type_error "expected a number" x)
  | t -> LspUtils.error2 "incompatible argument" t
*)
let do_plus env t = add env 0 t

let do_succ = function
  | Nb n -> Nb (succ n)
  | x -> LspUtils.type_error "num type expected" x

let do_pred = function 
  | Nb n -> Nb (pred n)
  | x -> x
(*      LspUtils.type_error "num type expected" x*)

let rec minus env n acc = function
  | NIL         -> Nb (n-acc)
  | Cons (x, l) ->
      (match eval env x with
      | Nb x -> minus env n (x+acc) l
      | y    -> LspUtils.type_error "num value expected" y)
  | y -> LspUtils.error2 "incompatible argument" y

let do_minus env t =
    match t with
    | Cons(x,l) ->
        (match eval env x with
        | Nb n -> minus env n 0 l
        | _ -> LspUtils.type_error "expected a number" x)
    | _ -> LspUtils.error2 "expected at least 2 args" t 

let do_mult env t = 
  let rec mult t acc =
    if is_null t then acc
    else mult (cdr t) ((LspUtils.int_of_cell (eval env (car t))) * acc)
  in Nb (mult t 1)

let do_div env t =
  let den = LspUtils.int_of_cell (do_mult env (cdr t)) in
  if den==0 then error "div by zero" 
  else Nb ((LspUtils.int_of_cell (eval env (car t)))/den)

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
        (match eval env x with
        | NIL -> NIL 
        | _ -> doand y)
    | t -> LspUtils.error2 "unexpected argument" t 
  in doand t

let do_or env t =
  let rec door = function
    | NIL -> NIL
    | Cons(x,y) ->
        (match eval env x with
        | NIL -> door y 
        | _ -> TRUE)
    | t -> LspUtils.error2 "unexpected argument" t 
  in door t

let do_not = function
  | NIL -> TRUE 
  | _   -> NIL


(* ************************************************************************** *)
(* STRINGS *)
let do_cat env t =
  let rec docat t acc =
    if is_null t then acc
    else docat (cdr t) (acc^(LspUtils.string_of_cell (eval env (car t)))) in
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
  match (eval env c) with
  | NIL -> acc
  | _ -> iterate env c e (eval env e)

let do_while env = function
  | Cons(c, Cons (e, NIL)) -> iterate env c e NIL
  | t -> LspUtils.error2 "expected 2 arguments" t

(* ************************************************************************** *)
(* IO *)

let do_println env t =
  let rec f t acc = 
    if is_null (cdr t) then 
      begin
        let s = eval env (car t) in
        print_endline (acc^(LspUtils.pp' s));
        s
      end
    else f (cdr t) (acc^(LspUtils.pp' (eval env (car t)))) in
  f t ""

let do_print env t =
   let rec f t acc = 
    if is_null (cdr t) then 
      begin
        let s = eval env (car t) in
        print_string (acc^(LspUtils.pp' s)); flush stdout;
        s
      end
    else f (cdr t) (acc^(LspUtils.pp' (eval env (car t)))) in
  f t ""

let do_write env t =
  let p = eval env (car t) in
  let s = eval env (cadr t) in
  if is_output_port p then
    if is_string s then
      begin
        let _,b,c = LspUtils.get_output_port p in
        let s' = LspUtils.string_of_cell s in
        (match (b,c) with
        | Some buf , None -> Buffer.add_string buf s'
        | None, Some c -> output_string c s'
        | _, _ -> assert false);
        TRUE
      end
    else LspUtils.type_error "expected to be a string" s
  else LspUtils.type_error "expected to be an output-port" p

let do_open_input_file env t =
  let fn = LspUtils.string_of_cell (eval env (car t)) in
  let p = 
    let c = open_in fn in
    let lb = Lexing.from_channel c in
    Port(Input(fn,lb,Some c)) in
  p

let do_open_input_string env t =
  let s = LspUtils.string_of_cell (eval env (car t)) in
  let lb = Lexing.from_string s in
  Port(Input("",lb,None))

let do_close_input_file env t =
  let p = eval env (car t) in
  if is_input_file p then
    begin
      let _,_, c = LspUtils.get_input_port p in
      close_in (Misc.always c);
      TRUE;
    end
  else LspUtils.type_error "expected to be an input_port" (car t)

let do_open_output_file env t =
  let fn = LspUtils.string_of_cell (eval env (car t)) in
  let p = 
    let c = open_out fn in
    Port(Output(fn,None,Some c)) in
  p

let do_open_output_string env t =
  let s = LspUtils.string_of_cell (eval env (car t)) in
  let buf = Buffer.create 80 in
  Buffer.add_string buf s;
  Port(Output("",Some buf,None))

let do_flush_output_string env t =
  let p = eval env (car t) in
  if is_output_string p then
    begin
      let _,b,_ = LspUtils.get_output_port p in
      Buffer.reset (Misc.always b);
      TRUE
    end
  else LspUtils.type_error "expected to be an output-string" (car t)

let do_get_output_string env t =
  let p = eval env (car t) in
  if is_output_string p then
    begin
      let _,b,_ = LspUtils.get_output_port p in
      Str(Buffer.contents (Misc.always b))
    end
  else LspUtils.type_error "expected to be an output-string" (car t)

let do_close_output_file env t =
  let p = eval env (car t) in
  if is_output_file p then
    begin
      let _,_,c = LspUtils.get_output_port p in
      close_out (Misc.always c);
      TRUE
    end
  else LspUtils.type_error "expected to be an input_port" (car t)

let do_read env t = 
    if is_null t then Lexer.build_object ()
    else 
      let _,lb,_ = 
        let p = eval env (car t) in
        if is_input_port p then LspUtils.get_input_port p 
        else LspUtils.type_error "expected to be an input port" p 
      in Lexer.build_object_lex lb

let do_read_line env t =
  let _,_,c = 
    let p = eval env (car t) in
    if is_input_port p then LspUtils.get_input_port p 
    else LspUtils.type_error "expected to be an input port" p in
  Str (input_line (Misc.always c))

(* ************************************************************************** *)
(* ENVIRONMENT *)

let do_make_env env t =
  let e = eval env (car t) in
  if is_symb e then 
    begin
      let s_e = LspUtils.symb_of_cell e in
      let env' = LspUtils.e_child !Globals.init_env 769 s_e in
      LspUtils.extend_global s_e (Env env') env;
      Globals.init_env := env';
      e
    end
  else 
    if is_env e then 
      begin
        Globals.init_env := LspUtils.env_of_cell e; 
        car t
      end
    else LspUtils.type_error "expected to be a symbol or environment expression" (car t)

let do_symbols t =
  let f l = 
    Misc.fold 
      (fun k l' -> Cons(Symb k, l')) 
      (Misc.reverse l) 
      NIL in
  f (LspUtils.symbols (LspUtils.env_of_cell t))

(* ************************************************************************** *)
(* EVALUATION *)
let do_eval env t =
  let res =
    match t with
    | Cons(Quote x,_) -> eval env x
    | Cons(x,_) -> let y = eval env x in eval env y
    | _ -> LspUtils.error2 "invalid argument" t in
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
  Nb (Sys.command (LspUtils.string_of_cell (eval env (car t))))
  
(* ************************************************************************** *)
(* EXIT *)
let do_exit _ _ = raise End_of_file

(* ************************************************************************** *)
(* PERVASIVES *)
let add_subr env s f = 
  LspUtils.extend_global 
    (LspUtils.symbol env s) 
    (Subr (Fn f)) 
    env

let add_subr1 env s f = 
  LspUtils.extend_global 
  (LspUtils.symbol env s) 
  (Subr (F1 f)) 
  env

let add_subr2 env s f = 
  LspUtils.extend_global 
    (LspUtils.symbol env s) 
    (Subr (F2 f)) 
    env

(*
let intern s =
  LspUtils.extend_global 
    (LspUtils.symbol !current_env s) 
    NIL 
    !current_env
*)

let init_global () =
  LspUtils.extend_global 
    (LspUtils.symbol !Globals.init_env "MAIN") 
    (Env !Globals.init_env)
    !Globals.init_env;
  add_subr !Globals.init_env "package" do_make_env;
  add_subr1 !Globals.init_env "symbols" do_symbols;
  add_subr !Globals.init_env "define" do_define;
  add_subr !Globals.init_env "defun" do_defun;
  add_subr !Globals.init_env "eval" do_eval;
  add_subr !Globals.init_env "set" do_set;
  add_subr !Globals.init_env "setq" do_setq;
  add_subr !Globals.init_env "let" do_let;
  add_subr2 !Globals.init_env "eq?" do_eq;
  add_subr2 !Globals.init_env "equal?" do_equal;
  add_subr1 !Globals.init_env "num?" do_nump;
  add_subr2 !Globals.init_env "cons" do_cons;
  add_subr1 !Globals.init_env "car" car;
  add_subr1 !Globals.init_env "cdr" cdr;
  add_subr !Globals.init_env "nth" do_nth;
  add_subr !Globals.init_env "list" do_list;
  add_subr !Globals.init_env "map" do_map;
  add_subr1 !Globals.init_env "null?" do_nullp;
  add_subr !Globals.init_env "+" do_plus;
  add_subr !Globals.init_env "-" do_minus;
  add_subr !Globals.init_env "*" do_mult;
  add_subr !Globals.init_env "/" do_div;
  add_subr2 !Globals.init_env ">" do_gt;
  add_subr2 !Globals.init_env "<" do_lt;
  add_subr2 !Globals.init_env ">=" do_gte;
  add_subr2 !Globals.init_env "<=" do_lte;
  add_subr !Globals.init_env "and" do_and;
  add_subr !Globals.init_env "or" do_or;
  add_subr1 !Globals.init_env "not" do_not;
  add_subr !Globals.init_env "cat" do_cat;
  add_subr !Globals.init_env "do" do_seq;
  add_subr !Globals.init_env "while" do_while;
  add_subr !Globals.init_env "exit" do_exit;
  add_subr !Globals.init_env "time" do_time;
  add_subr !Globals.init_env "print" do_print;
  add_subr !Globals.init_env "println" do_println;
  add_subr !Globals.init_env "write" do_write;
  add_subr !Globals.init_env "exec" do_exec;
  add_subr !Globals.init_env "read" do_read;
  add_subr !Globals.init_env "open-input-file" do_open_input_file;
  add_subr !Globals.init_env "open-input-string" do_open_input_string;
  add_subr !Globals.init_env "close-input-file" do_close_input_file;
  add_subr !Globals.init_env "close-input-string" do_close_input_file;
  add_subr !Globals.init_env "open-output-file" do_open_output_file;
  add_subr !Globals.init_env "open-output-string" do_open_output_string;
  add_subr !Globals.init_env "flush-output-string" do_flush_output_string;
  add_subr !Globals.init_env "get-output-string" do_get_output_string;
  add_subr !Globals.init_env "close-output-file" do_close_output_file;
  add_subr !Globals.init_env "read-line" do_read_line;
  add_subr !Globals.init_env "add-prop" do_addprop;
  add_subr !Globals.init_env "get-prop" do_getprop;
  add_subr !Globals.init_env "rem-prop" do_remprop;
  add_subr !Globals.init_env "get-plist" do_plist;
  add_subr !Globals.init_env "set-plist" do_set_plist;
  add_subr1 !Globals.init_env "succ" do_succ;
  add_subr1 !Globals.init_env "pred" do_pred;

