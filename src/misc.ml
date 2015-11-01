exception IncompatibleArgument;;

exception EmptyList;;

let always = function Some x -> x | _ -> raise IncompatibleArgument

let fold f l accum =
  let rec iter_list f l accum =
    match l with
      | e::tl -> iter_list f tl (f e accum)
      | [] -> accum in
  iter_list f l accum;;

let reverse l = fold (fun x l -> x::l) l [];;

let tail l =
  match l with
    [] -> raise EmptyList
    | _ :: tl -> tl;;

let split sep s =
  let len = String.length s in
  let cons i j acc =
    if i < j 
    then String.sub s i (j -i) :: acc
    else acc
  in
  let rec split_aux i j =
    if len <= j
    then cons i j []
    else
      if s.[j] == sep
      then cons i j (split_aux (succ j) (succ j))
      else split_aux i (succ j)
  in
  split_aux 0 0
