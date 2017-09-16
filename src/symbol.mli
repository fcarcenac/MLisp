type t = { 
  name : string;
  i : int; 
  scope : int;}

val compare : t -> t -> int

module Symb :
  sig
    type t = int * string
    val hash : t -> int
    val equal : t -> int * string -> bool
  end

module S :
  sig
    type key = Symb.t
    type 'a t = 'a Hashtbl.Make(Symb).t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
  end
type symbol_table = t S.t
val create : int -> symbol_table
val find : symbol_table -> S.key -> t
val add : symbol_table -> S.key -> t -> unit
val bucket_val : 'a -> 'b -> 'b list -> 'b list
val hvalues : symbol_table -> t list
