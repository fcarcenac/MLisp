open Globals
open Term

let version = "0.9"
let banner = 
"===========================================================================\n"
^ Sys.executable_name ^" - "^"v"^version^"\n"
^
"==========================================================================="

let read () =
  (*
  let s = input_line !input_channel in
  command := !command ^ s;
  *)
  Lexer.build_object ()

(*
let flush_command () = command := ""
*)

let backup_file = ".mlisp.bkp"

(*
let backup_session () = 
  let s = !command ^ "\n" in
  (*flush_command ();*)
  output_string history s
*)

let do_load t =
  try
    let n = 
      Term.string_of_cell (eval !Term.current_env t)
    in
    Lexer.reset_parser ();
    let chan = !input_channel in
    input_channel := open_in n;
    print_endline "Loading ...\n";
    Lexer.reset_parser();
    let res = 
      (try 
        while true do
          let term = Term.compile !Term.current_env (read ()) in
          ignore (eval !Term.current_env term);
        done;
        Term.NIL
      with 
      | End_of_file -> Term.TRUE
      | Term.Error -> Term.NIL) in
    print_endline (n^" has been successfully loaded");
    close_in !input_channel;
    input_channel := chan;
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
    try
      let term = read () in
      let term = Term.compile (!Term.current_env) term in
      begin
        try
          (* evaluates, then prints the result *)
          let s = eval !Term.current_env term in
          print_string "= ";
          Term.print s;

          (*
          (* backup the latest valid command *)
          backup_session ();
          *)
        with 
        | Term.Error -> Term.error2 "found an error" term
        | Not_found -> Term.error2 "unknown symbol in" term
        | Stack_overflow -> print_endline "stack overflow ..."
      end
    with 
    | Term.Error -> ()
    | e -> raise e
  done

let _ =
(*  Gc.set 
    {(Gc.get ()) with
      Gc.minor_heap_size = 2000*1024;
    };*)
  (* Initialize the interpreter *)
  Subr.init_global ();
  Subr.add_subr1 "load" do_load;
  
  (* Launch the REPL *)
  print_endline banner;
  try toplevel (); 
  with End_of_file -> 
    print_newline(); 
    print_endline "Bye"; 
    exit 0
 
