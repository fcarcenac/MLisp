exception IncompatibleArgument
exception EmptyList

val always : 'a option -> 'a

val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

val reverse : 'a list -> 'a list

val map : ('a -> 'b) -> 'a list -> 'b list

val iter : ('a -> unit) -> 'a list -> unit

(*val tail : 'a list -> 'a list*)

(*val ssplit : string -> string -> string list*)

val split : char -> string -> string list
