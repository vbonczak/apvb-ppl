%{
open Ast
%}

%start <expr> prog

%token <int> INT
%token <float> REAL
%token <string> ID
%token <string> STRING

%token EOL
%token SEMICOLON
%token COLON

%token LET
%token IN
%token DIST
%token PIPE
 
%token OPLEQ
%token OPGEQ
%token OPLT
%token OPGT

%token OPOR
%token OPAND

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
%token ENDIF
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
 %token PRINT
 


%token LEFTPAR 
%token RIGHTPAR 
%token LEFTBR
%token RIGHTBR

 %token UNIT

%token EOF

%nonassoc LEFTBR   LEFTPAR  
%nonassoc UNIT

%nonassoc IF
 
 
%left EQUALS

%left OPGEQ OPGT OPLEQ OPLT 
 
%nonassoc OPPLUS OPFPLUS OPFMOINS OPMOINS
%left OPDIV OPFDIV OPMULT OPFMULT


%right OPOR
%right OPAND


%%
prog:
  | xs =  list(statement) EOF { ast_of_list xs }
  ;
statement:
  | e = expr EOL+ { e } 
  | e = expr EOF { e }
expr: 
  | LET x = ID l = list(arg) EQUALS  EOL* e = expr  { Let (x,  l, e) } 
  | IF e = expr THEN et = expr ELSE ef = expr ENDIF { If(e, et, ef) }
  | LEFTBR l = separated_list(SEMICOLON, listel) RIGHTBR { Liste(l) }
  | LEFTPAR e = expr RIGHTPAR { e }
  | e = condition { e }
  | e1 = expr op = binop e2 = expr { Binop(op, e1, e2) }
  | BEGIN xs = list(statement) END { ast_of_list xs }
  | DIST x = ID EQUALS e1 = expr   { Dist (x, e1) } 
  | PPL_SAMPLE e = expr     { Proba (Sample, e) }
  | PPL_ASSUME e = expr     { Proba (Assume, e) }
  | PPL_INFER e = expr   { Proba (Infer, e) }
  | PPL_OBSERVE LEFTPAR e1 = expr PIPE e2 = expr; RIGHTPAR { Observe (e1, e2) } 
  | PPL_FACTOR e = expr   { Proba (Factor, e) }
  | PPL_METHOD s = ID  EOL { Method(s) }
  | PRINT s = STRING { Print(Text, s)}
  | PRINT id = ID {Print(Distrib, id)}
  | UNIT { Unit }
  | e = expr e2 = expr {App(e,e2)}
  | i = INT { Int i }
  | f = REAL { Real f }
  | x = ID { Var x }
  | s = STRING { String s }
arg:
  |x = ID { Var(x) }
  |UNIT {Unit}
listel:
  // |e = expr SEMICOLON {e}
  |e = expr {e}
binop:
  |OPOR{BOr}
  |OPAND{BAnd}
  |OPFDIV { DivF }
  |OPFMOINS {SubF}
  |OPFPLUS {AddF}
  |OPFMULT {MultF}
  |OPDIV { Div }
  |OPMOINS {Sub}
  |OPPLUS {Add}
  |OPMULT {Mult}
condition:
  |e1 = expr OPLT e2 = expr { Cond(LT, e1, e2) }
  |e1 = expr OPLEQ e2 = expr { Cond(Leq, e1, e2) }
  |e1 = expr OPGT e2 = expr { Cond(LT, e2, e1) }
  |e1 = expr OPGEQ e2 = expr { Cond(Leq, e2, e1) }
  |e1 = expr EQUALS e2 = expr { Cond(Eq, e1, e2) }
(*expr:
  | v = CAML; EOL; e = expr { StdCaml(v, e) }
  ;*)
  ;
