open Globals
open Subr

let version = "0.9"
let banner = 
"===========================================================================\n"
^ "MLisp"^" - "^"v"^version^"\n"
^
"==========================================================================="

let read () = Lexer.build_object !current_channel

let do_load env t =
  try
    let n = 
      Term.string_of_cell (snd (Term.eval_c !Term.current_env (Term.car t)))
    in
    Lexer.reset_parser ();
    let chan = !current_channel in
    current_channel := open_in n;
    print_endline "Loading ...\n";
    Lexer.reset_parser();
    let res = 
      (try 
        while true do
          let term = Term.compile !Term.current_env (read ()) in
          ignore (Term.eval_c !Term.current_env term);
        done;
        Term.NIL
      with 
      | End_of_file -> Term.TRUE
      | Term.Error -> Term.NIL) in
    print_endline (n^" has been successfully loaded");
    close_in !current_channel;
    current_channel := chan;
    Lexer.reset_parser ();
    res
  with
  | Sys_error s -> Term.error s

let toplevel () =
 Lexer.reset_parser ();
 while true do
    print_newline ();
    print_string ((Env.env_name !Term.current_env)^"> ");
    flush stdout;
    let term = Term.compile (!Term.current_env) (read ()) in
      (try
        let _, s = Term.eval_c !Term.current_env term in
        print_string "= ";
        Term.print s 
      with 
      | Term.Error -> Term.error2 "found an error" term
      | Not_found -> Term.error2 "unknown symbol" term
      | Stack_overflow -> print_endline "stack overflow ...")
 done

let _ =
  print_endline banner;
  init_global ();
  add_subr "load" do_load;
  try toplevel(); 
  with End_of_file -> print_newline(); print_endline "Bye"; exit 0
 
