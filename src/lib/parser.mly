%{
open Ast
%}

%start <expr> prog

%token <int> INT
%token <float> REAL
%token <string> ID
%token <string> CAML

%token EOL
%token SEMICOLON

%token LET
%token IN
%token DIST
%token PIPE
 
%token OPLEQ
%token OPGEQ
%token OPLT
%token OPGT

%token OPFPLUS
%token OPFMOINS
%token OPFDIV
%token OPFMULT

%token OPPLUS
%token OPMOINS
%token OPDIV
%token OPMULT

%token EQUALS
%token IF
%token THEN
%token ELSE
%token BEGIN
%token END

%token PPL_SAMPLE
%token PPL_ASSUME
%token PPL_INFER
%token PPL_OBSERVE
%token PPL_FACTOR
%token PPL_METHOD
 
 
%token LEFTPAR 
%token RIGHTPAR 
%token LEFTBR
%token RIGHTBR
 
%token EOF

%left OPPLUS OPFPLUS OPFMOINS OPMOINS
%left OPDIV OPFDIV OPMULT OPFMULT
 

%%
prog:
  | xs =  list(statement) EOF { ast_of_list xs }
  ;
statement:
  | e = expr EOL+ { e } 
  | e = expr EOF { e }
expr: 
  | LET x = ID l = list(arg) EQUALS e = expr IN { Let (x, l, e) } 
  | IF e = expr THEN et = expr ELSE ef = expr { If(e, et, ef) }
  | LEFTPAR e = expr RIGHTPAR { e }
  | LEFTBR l = list(expr) RIGHTBR { Liste(l) }
  | e = condition { e }
  | e1 = expr op = binop e2 = expr { Binop(op, e1, e2) }
  | BEGIN xs = list(statement) END { ast_of_list xs }
  | DIST x = ID EQUALS e1 = expr IN { Dist (x, e1) } 
  | PPL_SAMPLE e = expr   { Proba (Sample, e) }
  | PPL_ASSUME e = expr   { Proba (Assume, e) }
  | PPL_INFER e = expr { Proba (Infer, e) }
  | PPL_OBSERVE LEFTPAR e1 = expr PIPE e2 = expr; RIGHTPAR { Observe (e1, e2) }
  | PPL_FACTOR e = expr { Proba (Factor, e) }
  | PPL_METHOD s = ID { Method(s) }
  | i = INT { Int i }
  | f = REAL { Real f }
  | x = ID { Var x }
  | s1 = CAML { StdCaml(s1) }
  ;
arg:
  |x = ID { Var(x) }
listel:
  |e = expr SEMICOLON {e}
binop:
  |OPDIV { Div }
  |OPMOINS {Sub}
  |OPPLUS {Add}
  |OPMULT {Mult}
  |OPFDIV { DivF }
  |OPFMOINS {SubF}
  |OPFPLUS {AddF}
  |OPFMULT {MultF}
condition:
  |e1 = expr OPLT e2 = expr { Cond(LT, e1, e2) }
  |e1 = expr OPLEQ e2 = expr { Cond(Leq, e1, e2) }
  |e1 = expr OPGT e2 = expr { Cond(LT, e2, e1) }
  |e1 = expr OPGEQ e2 = expr { Cond(Leq, e2, e1) }
  |e1 = expr EQUALS e2 = expr { Cond(Eq, e1, e2) }
(*expr:
  | v = CAML; EOL; e = expr { StdCaml(v, e) }
  ;*)

