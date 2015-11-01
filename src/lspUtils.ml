open Lsp

module S = Symbol

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
  | Symb s -> s.S.name
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
  | Symb s -> s.S.name
  | Quote c -> "'"^(pp c)
  | Quasiquote c -> "`"^(pp c)
  | If(c,t,e) -> "(if "^(pp c)^" "^(pp t)^" "^(pp e)^")"
  | Unquote c -> ","^(pp c)
  | NIL -> "()"
  | Cons(x,y) -> "("^(pp_cell x y)^")"
  | Subr _ -> "<subr>"
  | Closure _ -> "<closure>"




(* ********************* ERROR HANDLING ***************************** *)
exception Error

let error2 msg e = 
  print_endline (msg^" - "^(pp e)); 
  raise Error

let type_error msg e = 
  print_endline ("type error: ");
  error2 msg e

let binding_error msg e = 
  print_endline ("binding error: ");
  error2 msg e

(* ************************************************************************ *)


let create sz = 
  Array.init sz (fun _ -> Stack.create ()) 

let init st vt s n =
  let dumval = {value = NIL ; plist =PropList.empty} in
  let dummy = dummy dumval in
  { 
    r1 = NIL ;
    r2 = NIL ;
    values = create vt ;
    pkg = {
      id = s ; 
      e_name = n ; 
      symbols = S.create st ; 
      father = dummy ;
      dummy = dummy ; }
  }

let e_child env st s =
  {
    r1 = env.r1 ; 
    r2 = env.r2 ;
    values = env.values ;
    pkg = {
      id = s.S.i ;
      e_name = s.S.name ;
      symbols = S.create st ;
      father = env.pkg ;
      dummy = env.pkg.dummy 
  }}

let env_name e = e.pkg.e_name

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

(*
let mov_r1 e x = { e with r1 = x}

let mov_r2 e x = { e with r2 = x}
*)

(*
let top e i = (Stack.top (Array.unsafe_get e.values i)).value
*)
let replace d x e =
  Stack.replace e (Array.unsafe_get d.values x.S.i)

let lookup e x =
  let stack = Array.get e.values x.S.i in
  if stack.Stack.c == []
  then raise Not_found
  else 
    match (Stack.top stack).value with
    | Env e -> e
    | _ -> type_error "expected an <env> value" (Symb x)

let lookup_plist e x = (find_rec e x.S.i).plist

let getprop e x p =
  try PropList.find p (lookup_plist e x)
  with Not_found -> binding_error "undefined property" (Symb p)

let addprop e x k v = 
  let c = find_rec e x.S.i in 
  replace e x {c with plist = PropList.add k v c.plist}

let rstprop e x =
  let c = find_rec e x.S.i in 
  replace e x {c with plist = PropList.empty}

let remprop e x k =
  let c = find_rec e x.S.i in 
  replace e x {c with plist = PropList.remove k c.plist}

let extend_global x y g =
  try
    replace g x { (find_rec g x.S.i) with value = y }
  with Not_found -> 
    replace g x { value = y ; plist = PropList.empty }

(*
let e_inherit env s = 
  { 
    r1 = env.r1 ;
    r2 = env.r2 ;
    values = env.values ;
    pkg = { 
      id = s.S.i ; 
      e_name = s.S.name ;
      symbols = S.create 93 ; 
      father = env.pkg ;
      dummy =  env.pkg.dummy}
  }
*)

let symbols d = S.hvalues d.pkg.symbols

let sanity_check e i =
  if i < Array.length (e.values) then ()
  else failwith "the value table needs to be resized"

let fresh = 
  let cpt = ref (-1) in 
  fun () -> incr cpt; !cpt

(** INCORRECT: the following program fails

(define x 3)
(package 'TOTO)
(define x 4)
(package MAIN)
x

should return 3

*)

let rec loc_symb e sc s f =
  if f == f.dummy then
    begin
      let id = fresh () in sanity_check e id;
      let ns = {S.name = s ; S.i = id ; S.scope = sc} in
      S.add e.pkg.symbols (sc,s) ns;
      ns
    end
  else
      try S.find f.symbols (f.id, s) 
      with Not_found -> loc_symb e sc s f.father 

let symbol e s = loc_symb e e.pkg.id s e.pkg

(* ************************************************************************ *)
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

(* ************************************************************************ *)
(* Ports Destructors *)
let get_input_port = function
  | Port(Input(x,y,z)) -> x,y,z
  | t -> type_error "expected to be an input port" t

let get_output_port = function
  | Port(Output(x,y,z)) -> x,y,z
  | t -> type_error "expected to be an output port" t

