exception IncompatibleArgument;;

exception EmptyList;;

let always = function Some x -> x | _ -> raise IncompatibleArgument

(** {4 fold} *)
(** 
{b Purpose} 

This function applies a function to all the elements of a list

{b Arguments}
- [f]: the function which has to be applied to the elements of the list
- [l]: the list over which iteration is performed
- [accum]: the initial value of the accumulator

*)
let fold f l accum =
  let rec iter_list f l accum =
    match l with
      | e::tl -> iter_list f tl (f e accum)
      | [] -> accum in
  iter_list f l accum;;
  
(** {4 length} *)
(** {b Purpose} 

This function computes the length of a list 

{b Arguments}
- [l]: the list of which {e length} is computed

*)
(*
let length l = fold (fun _ s -> s + 1) l 0;;
*)

(** {4 reverse} *)
(** {b Purpose} 

This function reverses the content of a list 

{b Arguments}
- [l]: the list which has to be reversed

*)
let reverse l = fold (fun x l -> x::l) l [];;

let tail l =
  match l with
    [] -> raise EmptyList
    | _ :: tl -> tl;;

let ssplit sep = Str.split (Str.regexp sep)
