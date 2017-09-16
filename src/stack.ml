(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

type 'a t = { mutable c : 'a list }

exception Empty

let create () = { c = [] }

let clear s = s.c <- []

let copy s = { c = s.c }

let push x s = s.c <- x :: s.c

let replace x s = 
  match s.c with
  | _ :: r -> s.c <- x :: r
  | []     -> s.c <- [x]

let pop s =
  match s.c with
  | _::tl -> s.c <- tl
  | []    -> s.c <- []

let top s =
  match s.c with
  |  hd::_ -> hd
  | []     -> raise Empty

let is_empty s = (s.c = [])

let rec lgth_aux i = function
  | [] -> i
  | _ :: tl -> lgth_aux (succ i) tl

let length s = lgth_aux 0 s.c

let rec iter_aux f = function
  | [] -> ()
  | x :: tl -> f x ; iter_aux f tl

let iter f s = iter_aux f s.c
