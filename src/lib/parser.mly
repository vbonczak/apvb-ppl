%{
open Ast
%}

%start <expr> prog

%token <int> INT
%token <string> ID
%token <string> CAML

(*%token BEGIN_CAML
%token END_CAML*)
%token LET
%token IN
%token DIST
%token PIPE
%token LBRACKET
%token RBRACKET
%token EQUALS
%token EOL

%token PPL_SAMPLE
%token PPL_ASSUME
%token PPL_INFER
%token PPL_OBSERVE
%token PPL_FACTOR
%token PPL_METHOD
 
 
%token EOF
%left EOL

%%
prog:
  | xs = statements; EOF { ast_of_list xs }
  ;
statements:
  | e = statement ; EOL ; t = list(statement) { e::t }
statement:
  | e = expr; EOL { e }
  | EOL { Nop }
expr:
  | LET; x = ID; EQUALS; e = expr; IN { Let (x, [], e) }
  | LET; x = ID; l = args; EQUALS; e = expr { Let (x, l, e) }
  | DIST; x = ID; EQUALS; e1 = expr { Dist (x, e1) }
  | PPL_SAMPLE; e = expr   { Proba (Sample, e) }
  | PPL_ASSUME; e = expr   { Proba (Assume, e) }
  | PPL_INFER; e = expr { Proba (Infer, e) }
  | PPL_OBSERVE; LBRACKET; e1 = expr;PIPE;e2 = expr; RBRACKET { Observe (e1, e2) }
  | PPL_FACTOR; e = expr { Proba (Factor, e) }
  | PPL_METHOD; s = ID { Method(s) }
  | i = INT { Int i }
  | x = ID { Var x }
  | s1 = CAML { StdCaml(s1) }
  ;
args:
  |h=arg; t = list(arg) { h::t }
arg:
  |x = ID { Var(x) }
(*expr:
  | v = CAML; EOL; e = expr { StdCaml(v, e) }
  ;*)

