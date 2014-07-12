{
(* Empty header section *)
open Parser
open Globals
}

(* Character class definitions *)

let BLANK = [' ' '\t' '\n']

let LINE = [^ '\n']* '\n'
let COMMENT = ';' LINE

let ALPHA_minus = 
  ['a'-'z' 'A'-'Z' '-' '+' '*' '/' '=' '?' '>' '<' ]

let USCORE = '_'
let ALPHA = 
  ['a'-'z' 'A'-'Z' 
     '-' '+' '*' '/'  
     '=' '_' '?' '>' '<' '.']

let NUM = ['0'-'9']

let ALPHANUMERIC = ALPHA | NUM

let WORD = ALPHA_minus ALPHANUMERIC*
let NUMBER = '-'? NUM+

let ANY = _

let TRUE = "#t"

let COLON = ':'
(*let PDOT = '.'*)
let DOT = " . "
let LPAR = '('
let RPAR = ')'
let QUOTE = '\''
let UNQUOTE = ','
let QUASIQUOTE = '`'
let NIL = "nil" | "NIL" | "Nil"

let STRING = '"' [^ '"' ]* '"'

(* Grammar rules *)

rule lisp_token = parse

  | BLANK               {lisp_token lexbuf}     (* Skip blanks and comments*)
  | COMMENT             {lisp_token lexbuf}

  | eof                 {raise End_of_file}     (* Give up on end of file *)

  | NUMBER
    { 
      let s = Lexing.lexeme lexbuf in Token_num(int_of_string s)
    }
(*  | PDOT                 {Token_pdot}*)
  | DOT                 {Token_dot}
  | TRUE                {Token_true}
  | QUOTE               {Token_quote}
  | QUASIQUOTE          {Token_quasiquote}
  | UNQUOTE             {Token_unquote}
  | LPAR                {Token_lpar}
  | RPAR                {Token_rpar}
  | NIL                 {Token_nil}
  | STRING      (* Remove the front and back double quotes *)
      { 
        let s = (Lexing.lexeme lexbuf) in 
        let il = String.index s '"'
        and ir = String.rindex s '"'
        in let ss = String.sub s (il+1) (ir-il-1)
        in Token_string(ss)
      }
  | WORD as lxm {Token_symbol lxm} 
  | ANY (* Default case: just skip the character *)
      {
        let s = (Lexing.lexeme lexbuf) 
        in (print_string ("Bad char: " ^s^ ". Continuing...\n");
            lisp_token lexbuf)
      }
      
{
(* Trailer section *) 

let lexing_buffer = 
  (* A global variable *)
  ref(Lexing.from_channel !current_channel)
;;

let build_object _ = 
  (* Channel is handled through the lexing buffer *)
  main lisp_token !lexing_buffer
;;

let build_object_lex lb =
  main lisp_token lb

let reset_parser () = 
  (* Rebuild the lexing buffer from the current channel *)
  lexing_buffer := (Lexing.from_channel !current_channel)
;;

}
