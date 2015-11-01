open Globals

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
      LspUtils.string_of_cell (Eval.eval !Globals.init_env t)
    in
    Lexer.reset_parser ();
    let chan = !input_channel in
    input_channel := open_in n;
    print_endline "Loading ...\n";
    Lexer.reset_parser();
    let res = 
      (try 
        while true do
          let term = Eval.compile !Globals.init_env (read ()) in
          ignore (Eval.eval !Globals.init_env term);
        done;
        Lsp.NIL
      with 
      | End_of_file -> Lsp.TRUE
      | LspUtils.Error -> Lsp.NIL) in
    print_endline (n^" has been successfully loaded");
    close_in !input_channel;
    input_channel := chan;
    Lexer.reset_parser ();
    res
  with
  | Sys_error s -> Eval.error s

let toplevel () =
  Lexer.reset_parser ();
  while true do
    print_newline ();
    print_string ((LspUtils.env_name !Globals.init_env)^"> ");
    flush stdout;
    try
      let term = read () in
      let term = Eval.compile (!Globals.init_env) term in
      begin
        try
          (* evaluates, then prints the result *)
          let s = Eval.eval !Globals.init_env term in
          print_string "= ";
          Eval.print s;

          (*
          (* backup the latest valid command *)
          backup_session ();
          *)
        with 
        | LspUtils.Error -> LspUtils.error2 "found an error" term
        | Not_found -> LspUtils.error2 "unknown symbol in" term
        | Stack_overflow -> print_endline "stack overflow ..."
      end
    with 
    | LspUtils.Error -> ()
    | e -> raise e
  done

let _ =
(*  Gc.set 
    {(Gc.get ()) with
      Gc.minor_heap_size = 2000*1024;
    };*)
  (* Initialize the interpreter *)
  Subr.init_global ();
  Subr.add_subr1 !Globals.init_env "load" do_load;
  
  (* Launch the REPL *)
  print_endline banner;
  try toplevel (); 
  with End_of_file -> 
    print_newline(); 
    print_endline "Bye"; 
    exit 0
 
