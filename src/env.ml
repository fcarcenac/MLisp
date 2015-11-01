module M = Misc

type t_symbol = {
  name  : string ; 
  i     : int ; 
  scope : int ;}

let name s = s.name
let id s = s.i
let scope s = s.scope

(* Symbols tables module *)
module Symb = struct
  type t = int * string
  let hash = Hashtbl.hash
  let equal = (=)
end

module S = Hashtbl.Make(Symb)

let bucket_val _ b c =  b :: c
let hvalues t = S.fold bucket_val t []

module Pkey = struct
  type t = t_symbol
  let compare (x:t_symbol) (y:t_symbol) = compare x.i y.i
end

module Values = struct
  type 'a t_values = ('a Stack.t) array
  
  let create sz = 
    Array.init sz (fun _ -> Stack.create ()) 
  
  let size a = Array.length a
  
  let find t i = 
    try Stack.top (Array.get t i)
    with Stack.Empty -> raise Not_found

  let push t i e = Stack.push (Array.get t i) e
 
  let pop t i = Stack.pop (Array.unsafe_get t i)

end

module PropList = Map.Make(Pkey)

type 'a package = { 
  id             : int ;
  e_name         : string ;
  mutable father : 'a package ; 
  mutable dummy  : 'a package ; 
  symbols        : t_symbol S.t ; }

type ('a, 'b) ext_t = {
    r1 : 'b ;
    r2 : 'b ;
    values : 'a Values.t_values ;
    pkg    : 'a package ; }

let dummy (_:'a) =
  let rec e : 'a package = { 
    id      = -1 ; 
    e_name  = "" ; 
    symbols = S.create 0;
    father  = e ;
    dummy   = e ; }
  in
  e

let init st vt s n x y = 
  let dummy = dummy y in
  { 
    r1 = x ;
    r2 = x ;
    values = Values.create vt ;
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
      id = s.i ;
      e_name = s.name ;
      symbols = S.create st ;
      father = env.pkg ;
      dummy = env.pkg.dummy 
  }}

let env_id e = e.pkg.id
let env_name e = e.pkg.e_name
let s_table e = e.pkg.symbols
let v_table e = e.values

let rec find_rec d x =
  let stack = Array.get d.values x.i in
  if Stack.is_empty stack
  then 
    begin
      let father = d.pkg.father in
      if father == d.pkg.dummy then raise Not_found
      else find_rec {d with pkg = father} x
    end
  else Stack.top stack

let rec r_get d x =
  if Stack.is_empty (Array.get d.values x.i)
  then
    begin
      let father = d.pkg.father in
      if father == d.pkg.dummy then raise Not_found
      else r_get {d with pkg = father} x
    end
  else d, x.i

let top e i = Stack.top (Array.unsafe_get (e.values) i)

let push e i x = Stack.push x (Array.unsafe_get e i)

let replace d x e =
  let stack = Array.unsafe_get d.values x.i in
  Stack.replace e stack


let find d x = 
  let stack = Array.get d.values x.i in
  if Stack.is_empty stack
  then raise Not_found
  else Stack.top stack


let get d x = 
  if Stack.is_empty (Array.get d.values x.i)
  then raise Not_found
  else d, x.i

let e_inherit env s = 
  { 
    r1 = env.r1 ;
    r2 = env.r2 ;
    values = env.values ;
    pkg = { 
      id = s.i ; 
      e_name = s.name ;
      symbols = S.create 93 ; 
      father = env.pkg ;
      dummy =  env.pkg.dummy}
  }

let symbols d = hvalues d.pkg.symbols

let sanity_check e i =
  if i < Array.length (e.values) then ()
  else failwith "the value table needs to be resized"

let fresh = 
  let cpt = ref (-1) in 
  fun () -> incr cpt; !cpt

let rec loc_symb e s f =
if f == f.dummy then
  begin
    let sc = e.pkg.id in
    let id = fresh () in sanity_check e id;
    let ns = {name = s ; i = id ; scope = sc} in
    S.add e.pkg.symbols (sc,s) ns;
    ns
  end
else
    try S.find (f.symbols) (f.id, s) 
    with Not_found -> loc_symb e s f.father 

let symbol e s = loc_symb e s e.pkg
