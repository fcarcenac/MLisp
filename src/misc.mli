exception IncompatibleArgument
exception EmptyList

val always : 'a option -> 'a

val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

val reverse : 'a list -> 'a list

val tail : 'a list -> 'a list

(*val ssplit : string -> string -> string list*)

val split : char -> string -> string list
