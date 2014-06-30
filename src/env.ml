open Hash

module M = Misc

type t_symbol = {name : string; i: int; scope: int}

let name s = s.name
let id s = s.i
let scope s = s.scope
(*
let pp s =
  let pp_n = "{name = "^s.name
  and pp_i = " ; id = "^(string_of_int s.i)
  and pp_sc = " ; scope = "^(string_of_int s.scope) in
  pp_n^pp_i^pp_sc
*)

module Symb = struct
  type t_hashed = int * string
  let hash = Hashtbl.hash
  let equal = (=)
  let compare = compare
end

module S = MakeHashTable(Symb)

module Pkey = struct
  type t = t_symbol
  let compare (x:t_symbol) (y:t_symbol) = (x.i) - (y.i)
end

module O = struct
  module A = Array

  type 'a t_array = ('a list) array 
  
  let create sz = A.make sz []
  
  let size = A.length 
  
  let find t i =
    match (A.get t i) with 
    | x::_ -> x 
    | _ -> raise Not_found

  let add t i e = A.set t i (e::(A.get t i))

  let copy = A.copy
  
  let replace t i e = 
    match (A.get t i) with
    | _::tl -> A.set t i (e::tl)
    | _ -> () 
  
  let remove t i =
    match (A.get t i) with
    | _::tl -> A.set t i tl
    | _ -> ()
  
  let keys t = 
    let _, l = 
      A.fold_right (fun x (n,a) -> (n+1, if x=[] then a else n::a)) t (0,[]) 
    in l
end

(*
let values = (ObjPool.create 769 : 'a ObjPool.t_array)
let symbols = (Symbols.create 769 : symbol Symbols.t_hashtable)
*)

module PropList = Map.Make(Pkey)

type 'a ext_t = 
    { id  : int ;
    e_name : string ;
    symbols : (t_symbol S.t_hashtable);
    values : ('a O.t_array);
    chain : (('a ext_t) ref) list }

(*
let pp_env e = 
  "env id = "^(string_of_int e.id)^"\n"^
  "env chain = "
    ^(Misc.fold (fun i acc -> acc^(string_of_int i)^" ") e.chain "[")^"]\n"
*)

let init st vt s n = 
  {id = s ; 
  e_name = n ; 
  symbols = S.create st ; 
  values = O.create vt ; 
  chain = []}

let env_id e = e.id
let env_name e = e.e_name
let s_table e = (e.symbols)
let v_table e = (e.values)
let chain e = e.chain
let find d x = O.find d.values x.i
let find_id d i = O.find d.values i

let create env s = 
  { id = s.i ; 
    e_name = s.name ;
    symbols = S.create 93 ; 
    values = env.values ; 
    chain = (ref env)::env.chain }

let add d x = O.add d.values x.i
let remove d x = O.remove d.values x.i
let replace d x = O.replace d.values x.i
let symbols d = S.values d.symbols

let copy e =
  { id = e.id ;
    e_name = e.e_name ;
    symbols = (S.copy (e.symbols)) ; 
    values = (O.copy (e.values));
    chain = e.chain }

let sanity_check e i =
  if i < O.size (e.values) then ()
  else failwith "the value table needs to be resized"

let fresh = let cpt = ref (-1) in fun () -> incr cpt; !cpt

let symbol e s =
  let s_tbl = e.symbols
  and sc = env_id e in
  let chain = (ref e)::(e.chain) in
  let rec loc_symb = function
    | [] ->
        let id = fresh () in sanity_check e id;
        let ns = {name = s ; i = id ; scope = sc} in
        S.add s_tbl (sc,s) ns;
        ns
    | e::tl -> 
        try 
          let env = !e in
          S.find (env.symbols) (env.id, s) 
        with Not_found -> loc_symb tl in
  loc_symb chain

