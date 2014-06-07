(* $Author: frca $ *)
(* $Date: 2013-03-18 08:29:46 $ *)
(* $Revision: 1.14 $ *)
(* *)
(* *)
(* Copyright (C) 2008-2009 Prover Technology SAS, All Rights Reserved *)
(* ****************************************************************** *)

(** 
The module defines the types, functions, modules and functors 
required to implement instanciated HashTables
This module defines a generic interface for HashTable :
		- the type of hashtables key is a parameter of HashTable
		- the type of stored elements is a parameter of HashTable

@author François Carcenac
*)

(**{1 Parameterized HashTables}*)

(**{2 Provided services}
	- facilities for the definition of hashtables
*)

(**{2 Dependencies}	
	- {!Set}
*)

module type HashInstanciation =
  sig
    type t_hashed
    
    val equal : t_hashed -> t_hashed -> bool
    val compare : t_hashed -> t_hashed -> int
    val hash : t_hashed -> int
  end;;



(**{4 GenericHashTable}*)
(** This module gives a specification for parameterized hashtables. \
It describes the services offered to deal with hashtables*) 
module type GenericHashTable =
	sig
	(** type specification: hashtable keys*)
    	type t_key
    	(* type specification: hashtable elements*)
    	
    	(** type specification: hashtable*) 
         type 'a t_hashtable

    	module OrderedKey : Set.OrderedType
    	module KeySet : Set.S
    	(** specification of a module *)

    	(** Given an initial size argument, creates a new hashtable*)
    	val create : int -> 'a t_hashtable
(*    	(** Erases the elements of the hashtable*)*)
(*    	val clear : 'a t_hashtable -> unit        *)

    	(** Copy the hashtable into a new one*)
    	val copy : 'a t_hashtable -> 'a t_hashtable

    	(**
    	Adds a new element into the hashtable. The element is specified by its key 
      and its value. The hashtable is resized if needed.*)
    	val add : 'a t_hashtable -> t_key -> 'a -> unit
    	(**Removes an element from the hashtable. The element is specified by its
      key.*) 
    	val remove : 'a t_hashtable -> t_key -> unit

    	(**Returns the keys list of a hashtable*)
    	val keys : 'a t_hashtable -> t_key list

    	(**Returns the elements contained in the hashtable*)
    	val values : 'a t_hashtable -> 'a list
(*    	(**Given a predicate 'abstraction', the function returns the keys of the *)
(*      elements that match the predicate*)                                      *)
    	val filter : 'a t_hashtable -> ('a -> bool) -> t_key list

    	(**Given a key k, the function retrieves the element of key k if it 
      exists. If not, the function throws an exception 'Not_found'*) 
    	val find : 'a t_hashtable -> t_key -> 'a

    	(**Given a key k, the function returns the list of elements of key k.*)
    	val find_all : 'a t_hashtable -> t_key -> 'a list

    	(** Replaces the element of key k with a new value*)
    	val replace : 'a t_hashtable -> t_key -> 'a -> unit
    	(**Tests if a key is defined in the hashtable*)
    	val mem : 'a t_hashtable -> t_key -> bool

    	(**Returns the length of the hashtable*)
    	val length : 'a t_hashtable -> int

(*      val count_empty : 'a t_hashtable -> unit -> unit *)
    	(**{e fold} iteration over a hashtable*)
    	val fold : (t_key -> 'a -> 'b -> 'b) -> 'a t_hashtable -> 'b -> 'b
    	val iter : (t_key -> 'a -> unit) -> 'a t_hashtable -> unit
	end

module HashedString : HashInstanciation with type t_hashed = string

module MakeHashTable(K : HashInstanciation) : 
  GenericHashTable with type t_key = K.t_hashed

(**{4 String Hashtable}*)
module Dictionary: GenericHashTable with type t_key=string

(**{4 Int Hashtable}*)
module IntTable: GenericHashTable with type t_key=int
