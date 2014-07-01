let bernstein_hash s =
  let rec bernsteinHash i j h =
    if (i=j) then h
    else bernsteinHash (i+1) j (((h lsl 5) + h ) + (int_of_char s.[i])) in
  bernsteinHash 0 (String.length s) 5381;;

let id x = x;;

(** {4 t} *)
(** {4 Purpose} the type defines a model of hash table. It is defined by its 
size, which denotes the number of buckets, and a bucket list array*)
type ('a, 'b) t =
  { mutable size: int;                        (** number of elements *)
    mutable data: ('a, 'b) bucketlist array } (** the buckets *)

(** {4 bucketlist} *)
(** {4 Purpose} the type defines a model of buckets lists which is a sub 
structure used in the definition of hash table.*)
and ('a, 'b) bucketlist =
    Empty                                  (** no bucket is defined *)
  | Cons of 'a * 'b * ('a, 'b) bucketlist  (** the constructor adds a new bucket to the list *)

(** {4 create} *)
(**
{4 Purpose} the function creates a table with an initial size.
{4 Arguments}
   - initial size of the table

 *) 
let create initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0; data = Array.make s Empty }

(*(** {4 clear} *)                            *)
(*(**                                         *)
(*{4 Purpose} the function clears the content *)
(*{4 Arguments}                               *)
(*   - table                                  *)
(*                                            *)
(* *)                                         *)
(*let clear h =                               *)
(*  for i = 0 to Array.length h.data - 1 do   *)
(*    h.data.(i) <- Empty                     *)
(*  done;                                     *)
(*  h.size <- 0                               *)

(** {4 copy} *)
(**
{4 Purpose} the function copy the content of a ('a,'b) t 
{4 Arguments}
   - table

 *)
let copy h =
  { size = h.size;
    data = Array.copy h.data }

(** {4 length}*)
(**
{4 Purpose} the function returns the length of a table
{4 Arguments}
   - table 

 *)
let length h = h.size

(** {4 resize} *)
(**
{4 Purpose} the function resizes a table
{4 Arguments}
   - hash function
   - table

 *)
let resize hashfun tbl =
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = min (2 * osize + 1) Sys.max_array_length in
  if nsize <> osize then begin
    let ndata = Array.create nsize Empty in
    let rec insert_bucket = function
      | Empty -> ()
      | Cons(key, data, rest) ->
          insert_bucket rest; (* preserve original order of elements *)
          let nidx = (hashfun key) mod nsize in
          ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    tbl.data <- ndata;
  end

(** {4 iter} *)                                                                
(**                                                                            
{4 Purpose} the function implements an iterator over the structure of the table
{4 Arguments}                                                                  
   - function that has to be applied to all elements                          
   - table
*)
let iter f h =
  let rec do_bucket = function
    | Cons(k, d, rest) -> f k d; do_bucket rest
    | Empty -> () in 
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

(** {4 fold} *)
(** 
{4 Purpose} the function implements a fold iterator applied to the buckets of a 
table 
{4 Arguments}
   - function that has to be applied
   - table
   - initial value of the iteration

 *)
let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty -> accu
    | Cons(k, d, rest) -> do_bucket rest (f k d accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

(**{2 Sub-modules}*)
(**{4 ElemInstanciation}*)
(** This module gives a specification for hashtable elements. 
Each module that instanciates GenericHashTable shal satisfy this specification*)
(*module type ElemInstanciation =
  sig
        (** {4 t_elem} *)
        (** {4 Purpose} the type defines an abstract model of buckets values *)
    type t_elem
  end;;
*)
(**{4 HashInstanciation}*)
(** This module gives a specification for hashtable keys. 
It means that each module supposed to instanciate hashtable keys must satisfy 
typing constraints.*)
module type HashInstanciation =
  sig
    
        (** {4 t_hashed} *)
        (** {4 Purpose} specification of hashed elements *)
        type t_hashed
        
        (** {4 equal} *)
        (** {4 Purpose} tests if two hashed elements are equal*)
    val equal : t_hashed -> t_hashed -> bool
    
        (** {4 compare} *)
        (** {4 Purpose} compares two hashed elements (implements a relation 
    order over hashed elements)*)
    val compare : t_hashed -> t_hashed -> int
    
        (** {4 hash} *)
        (** {4 Purpose} hash function over hashed elements*)
    val hash : t_hashed -> int
  end;;

(**{4 GenericHashTable}*)
(* This module gives a specification for parameterized hashtables. 
It describes the services offered to deal with hashtables*)
module type GenericHashTable =
  sig
        (* {4 t_key} *)
        (* {4 Purpose} type specification: hashtable keys*)
      type t_key
       
        (* {4 t_elem} *)
        (* {4 Purpose} type specification: hashtable elements*)
        (*type t_elem*)
        
        (* {4 t_hashtable} *)
        (* {4 Purpose} type specification: hashtable*)      
                        type 'a t_hashtable
                          
        (* {4 OrderedKey} *)
        (* {4 Purpose} *)
        module OrderedKey : Set.OrderedType
      
        (* {4 KeySet} *)
        (* {4 Purpose} the module specifies the sets of keys *)
        module KeySet : Set.S
          
        (* {4 create} *)
        (* {4 Purpose} given an initial size argument, creates a new 
        hashtable*)
      val create : int -> 'a t_hashtable
       
(*        (** {4 clear} *)                                      *)
(*        (** {4 Purpose} Erases the elements of the hashtable*)*)
(*      val clear : 'a t_hashtable -> unit                      *)
       
        (* {4 copy} *)
        (* {4 Purpose} copy the hashtable into a new one*)
      val copy : 'a t_hashtable -> 'a t_hashtable
        
        (* {4 add} *)
        (* {4 Purpose} adds a new element into the hashtable. The element is 
        specified by its key and its value. The hashtable is resized if 
        needed.*)
      val add : 'a t_hashtable -> t_key -> 'a -> unit
       
        (** {4 remove} *)
        (** {4 Purpose} removes an element from the hashtable. The element is 
        specified by its key.*)
      val remove : 'a t_hashtable -> t_key -> unit
       
        (* {4 keys} *)
        (* {4 Purpose} returns the keys list of a hashtable*)
      val keys : 'a t_hashtable -> t_key list
       
        (* {4 values} *)
        (* {4 Purpose} returns the elements contained in the hashtable*)
      val values : 'a t_hashtable -> 'a list
       
(*        (** {4 filter} *)                                                     *)
(*        (** {4 Purpose} given a predicate 'abstraction', the function returns *)
(*        the keys of the elements that match the predicate*)                   *)
      val filter : 'a t_hashtable -> ('a -> bool) -> t_key list
       
        (* {4 find} *)
        (* {4 Purpose} given a key k, the function retrieves the element of 
        key k if it exists. If not, the function throws an exception 
        'Not_found'*)
      val find : 'a t_hashtable -> t_key -> 'a
       
        (* {4 find_all} *)
        (* {4 Purpose} given a key k, the function returns the list of elements 
        of key k.*)
      val find_all : 'a t_hashtable -> t_key -> 'a list
       
        (* {4 replace} *)
        (* {4 Purpose} replaces the element of key k with a new value *)
      val replace : 'a t_hashtable -> t_key -> 'a -> unit
       
        (** {4 mem} *)                                               
        (** {4 Purpose} tests if a key is defined in the hashtable *)
      val mem : 'a t_hashtable -> t_key -> bool                      
       
        (* {4 length} *)
        (* {4 Purpose} returns the length of the hashtable *)
      val length : 'a t_hashtable -> int
      
(*      val count_empty : 'a t_hashtable -> unit -> unit *)
        (* {4 fold} *)
        (* {4 Purpose} {e fold} iteration over a hashtable*)
      val fold : (t_key -> 'a -> 'b -> 'b) -> 'a t_hashtable -> 'b -> 'b
      val iter : (t_key -> 'a -> unit) -> 'a t_hashtable -> unit
  end;;

(**{2 Functors}*)
(**{4 MakeHashTable}*)
(** This functor gives a specification for GenericHashTable instanciation.*)
module MakeHashTable(K : HashInstanciation) : GenericHashTable 
with type t_key = K.t_hashed =
  struct
        (** {4 t_key} *)
        (** {4 Purpose} the type specifies the instanciation scheme for hash 
        table keys *)
        type t_key = K.t_hashed
        
        (** {4 t_elem} *)
        (** {4 Purpose} the type specifies the instanciation scheme for hash 
        table buckets *)
        (*type t_elem = 'a*)
        
        (** {4 t_hashtable} *)
        (** {4 Purpose} the type specifies the instanciation scheme for 
        hashtables *)
        type 'a t_hashtable = (t_key, 'a) t
      
       (** {4 OrderedKey} *)
       (** {4 Purpose} the module specifies the instanciation scheme of 
       OrderedType*)
      module OrderedKey =
       struct
                (** {4 t_key} *)
                (** {4 Purpose} the type *)
          type t = t_key
         
                (** {4 compare} *)
                (** {4 Purpose} the function compares two {!t_key} elements *)
          let compare = compare
        end
      
        (** {4 KeySet} *)
        (** {4 Purpose} the module defines {!t_key} sets *)
      module KeySet = Set.Make(OrderedKey)
      
        (** {4 create} *)
        (**
        {4 Purpose} the function creates an empty table of which length equals 
        the size argument
        {4 Arguments}
           - initial size of the table
           
         *)
      let create = create
        
(*         (** {4 clear} *)                                      *)
(*        (**                                                    *)
(*        {4 Purpose} the function removes the content of a table*)
(*        {4 Arguments}                                          *)
(*           - table                                             *)
(*                                                               *)
(*         *)                                                    *)
(*      let clear = clear                                        *)
        
        (** {4 copy} *)
        (**
        {4 Purpose} the function returns a table of which content is equal to 
        {4 Arguments}
           - table
           
         *)
      let copy = copy

         (** {4 safehash} *)
        (**
        {4 Purpose} the function computes a hash value of a key. The function 
        ensures that the result is in {e Z/max_int*Z}
        {4 Arguments}
           - key to be hashed
           
         *)
      let safehash key = (K.hash key) land max_int
      
      let l_add h b i =
        h.data.(i) <- b;
        h.size <- succ h.size;
        if h.size > Array.length h.data lsl 1 then resize safehash h 

        (** {4 add} *)
        (**
        {4 Purpose} the function adds an element [e] of key [k] in a table [t]
        {4 Arguments}
           - table [t]
           - key [k]
           - element [e]
           
         *)
      let add h key info =
        let i = (safehash key) mod (Array.length h.data) in
        l_add h (Cons(key, info, h.data.(i))) i

        (** {4 remove}                                               *)
        (** 
         {4 Purpose} the function removes the element specified by its key value         {4 Arguments}
           - table
           - key value                                                          *)
      let remove h key =
        let rec remove_bucket = function
          | Empty -> Empty
          | Cons(k, i, next) ->
              if K.equal k key then begin h.size <- pred h.size; next end
              else Cons(k, i, remove_bucket next) in
        let i = (safehash key) mod (Array.length h.data) in
        h.data.(i) <- remove_bucket h.data.(i)
        
       (** {4 getBucketKey} *)
        (**
        {4 Purpose} the function returns the 
        {4 Arguments}
           - key value
           - 
           
         *)
        let getBucketKey k _ accum = 
          if (KeySet.mem k accum) then accum else (KeySet.add k accum)
        
         (** {4 keys} *)
         (**
         {4 Purpose} the function returns the list of key values
         {4 Argument}
            - table
         
          *)
        let keys h = KeySet.elements (fold getBucketKey h KeySet.empty)
        
        let getBucketValue _ e accu = e::accu
        
         (** {4 values} *)
         (**
         {4 Purpose} the function returns the list of the table elements
         {4 Arguments}
            - table
        
          *)
        let values h = fold getBucketValue h []
        
        let getFiltered p k e accum = if (p e) then k::accum else accum
(*                                                                               *)
(*         (** {4 filter} *)                                                     *)
(*         (**                                                                   *)
(*         {4 Purpose} the function returns the elements of a table that satisfy *)
(*         a given predicate                                                     *)
(*         {4 Arguments}                                                         *)
(*            - table                                                            *)
(*            - predicate abstraction                                            *)
(*                                                                               *)
(*          *)                                                                   *)
        let filter h predicate = fold (getFiltered predicate) h []

         (** {4 find_rec} *)
         (**
         {4 Purpose} the function recursively searches for an element 
         (specified by its key) in the into a bucket list 
         {4 Arguments}
            - key value
            - bucket list
          *)
      let rec find_rec key = function
          Empty ->
              raise Not_found
          | Cons(k, d, rest) ->
              if K.equal key k then d else find_rec key rest
        
        (** {4 find} *)
        (**
        {4 Purpose} the function searches for an element (specified by its key) 
        into the table. If the element does not exist the function raises a 
        {!Not_found} exception. If several elements match, the function returns 
        the first one
        {4 Arguments}
           - table
           - key
      
         *)

(*      let count_empty h =
        let c = ref 0 in
        fun () ->
          for i=0 to Array.length h.data - 1 do
            if h.data.(i) = Empty then incr c;
          done;
          print_endline ("NB Empty buckets: "^(string_of_int !c))
*)
      let find h key =
         match h.data.((safehash key) mod (Array.length h.data)) with
           | Empty -> 
               raise Not_found
           | Cons(k1, d1, rest1) ->
             if K.equal key k1 then d1 else
               match rest1 with
               | Empty -> raise Not_found
               | Cons(k2, d2, rest2) -> 
                   if K.equal key k2 then d2 else
                    match rest2 with
                    | Empty -> raise Not_found
                    | Cons(k3, d3, rest3) ->
                       if K.equal key k3 then d3 else find_rec key rest3
        
        (** {4 find_all} *)
        (**
        {4 Purpose} the function searches for all the elements that match a key 
        [k]. 
        {4 Arguments}
           - table
           - key [k]
      
         *)
      let find_all h key =
          let rec find_in_bucket = function
            Empty -> []
            | Cons(k, d, rest) ->
                if K.equal k key
                then d :: find_in_bucket rest
                else find_in_bucket rest in
          find_in_bucket h.data.((safehash key) mod (Array.length h.data))
        
        (** {4 replace} *)
        (**
         {4 Purpose} the function replaces an element of key [k] in a table. 
        If the element does not exist, then the function adds it to the table
         {4 Arguments}
           - table
           - key [k]
           - element
      
         *)
      let replace h key info =
          let rec replace_bucket = function
            | Empty -> raise Not_found
            | Cons(k, i, next) ->
                if K.equal k key then Cons(k, info, next)
                else Cons(k, i, replace_bucket next) in
          let i = (safehash key) mod (Array.length h.data) in
            let l = h.data.(i) in
              try h.data.(i) <- replace_bucket l
              with Not_found -> l_add h (Cons(key, info, l)) i

        (** {4 mem}                                                        *)
        (**                                                                  
        {4 Purpose} the function tests if an element of key [k] belongs to a 
        table                                                                
        {4 Arguments}                                                        
           - table                                                           
           - key [k] of the element                                          
                                                                             
                                                                           *)
      let mem h key =                                                        
          let rec mem_in_bucket = function                                   
            | Empty -> false                                               
            | Cons(k, _, rest) -> K.equal k key || mem_in_bucket rest in       
          mem_in_bucket h.data.((safehash key) mod (Array.length h.data))    
      
        (** {4 iter} *)
        (** {4 Purpose} the function specializes the polymorphic function 
        {!iter} *)
      let iter = iter

        (** {4 fold} *)
        (** {4 Purpose} the function specializes the polymorphic function 
        {!foldr} *)
      let fold = fold
        
       (** {4 length} *)
       (**
        {4 Purpose} the function returns the length of the table
        {4 Arguments}
          - table
      
        *)
      let length = length
    end;;

(** {2 Dictionary} *)

module HashedString =
struct
  type t_hashed = string
  let equal = (=)
  let compare = compare
  let hash = (*Hashtbl.hash*) bernstein_hash
end;;

module HashedInt = 
struct
  type t_hashed = int
  let equal = (=)
  let compare = compare
  let hash = id
end;;

module StrTable = MakeHashTable(HashedString);;

module IntTable = MakeHashTable(HashedInt);;
