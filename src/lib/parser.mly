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
%left LET

%%
prog:
  | xs =  list(statement) EOF { ast_of_list xs }
  ;
statement:
  | e = expr EOL+ { e } 
expr:
  | LET x = ID l = list(arg) EQUALS e = expr IN { Let (x, l, e) } 
  | DIST x = ID EQUALS e1 = expr { Dist (x, e1) }
  | PPL_SAMPLE e = expr   { Proba (Sample, e) }
  | PPL_ASSUME e = expr   { Proba (Assume, e) }
  | PPL_INFER e = expr { Proba (Infer, e) }
  | PPL_OBSERVE LBRACKET e1 = expr PIPE e2 = expr; RBRACKET { Observe (e1, e2) }
  | PPL_FACTOR e = expr { Proba (Factor, e) }
  | PPL_METHOD s = ID { Method(s) }
  | i = INT { Int i }
  | x = ID { Var x }
  | s1 = CAML { StdCaml(s1) }
  ;
arg:
  |x = ID { Var(x) }
(*expr:
  | v = CAML; EOL; e = expr { StdCaml(v, e) }
  ;*)

