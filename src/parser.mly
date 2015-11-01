/* File parser.mly */

%{
module E = Env
module M = Misc

let rec mk_path e = function
  | [x] -> Term.Symb(E.symbol e x)
  | x::l ->
      let sx = E.symbol e x in
      begin
        try
          let ne = Term.lookup e sx in
          Term.Path(Term.Symb sx, mk_path ne l)
          with Not_found -> 
            Term.error2 "unknown symbol" (Term.Symb sx)
      end
  | [] -> raise Parse_error

%}

/* Declarations */

%token <int> Token_num
%token <string> Token_symbol
%token <string> Token_string
%token Token_pdot
%token Token_lpar Token_rpar 
%token Token_nil
%token Token_quote
%token Token_quasiquote
%token Token_unquote
%token Token_true
%token Token_dot

%start main             /* the entry point */
%type  <Term.cell> main

%% 
/* Grammar rules */

main: 
  | expr          {$1}

expr:
  | expr_atom     { $1 }
  | expr_list     { $1 }

expr_atom:
  /* First argument of M.ssplit is a regexp, Take car with special characters */
  | Token_symbol  { 
      let l = M.split '.' $1 in 
      mk_path  (!Term.current_env) l}
  | Token_nil     { Term.NIL }
  | expr_val                { $1 }

expr_val:
  | Token_true              { Term.TRUE }
  | Token_num               { Term.Nb $1 }
  | Token_string            { Term.Str $1 }

expr_list:
  | Token_lpar par_expr Token_rpar  { $2 }
  | Token_quote expr                { Term.Quote $2 }
  | Token_quasiquote expr           { Term.Quasiquote $2 }
  | Token_unquote expr              { Term.Unquote $2 }

par_expr:
  | expr Token_dot expr             { Term.Cons($1,$3) }
  | list_expr                       { $1 }

list_expr:
  | /* Nothing */                   { Term.NIL }
  | expr list_expr                  { Term.Cons($1,$2) }
/*
id:
  | Token_symbol {$1}

path:
  | path_               { $1 }

path_:
  | id                  { [$1] }
  | id Token_pdot path_ { $1::$3 }
*/
%% 

(* Empty Ocaml trailer section *)




