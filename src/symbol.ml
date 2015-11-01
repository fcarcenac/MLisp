type t = {
  name  : string ; 
  i     : int ; 
  scope : int ; }

let compare (x: t) y = compare x.i y.i

(* Symbols tables module *)
module Symb = struct
  type t = int * string
  let hash (x : t) = Hashtbl.hash x
  let equal (x : t) y = x = y
end

module S = Hashtbl.Make(Symb)

type symbol_table = t S.t

let create n = S.create n
let find t k = S.find t k
let add t k v = S.add t k v

let bucket_val _ b c =  b :: c
let hvalues t = S.fold bucket_val t []


