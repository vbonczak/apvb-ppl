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
%token POINT

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

%token ASSIGN
%token EQUALS

%token FOR
%token TO
%token DO
%token DONE

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

%nonassoc SEMICOLON
%nonassoc MoinsSeul 
%nonassoc Binop
%left Application

%nonassoc LEFTBR   LEFTPAR   POINT

%nonassoc IF
%nonassoc FOR 

%nonassoc ASSIGN
%left EQUALS

%left OPGEQ OPGT OPLEQ OPLT 
 

%nonassoc OPPLUS OPFPLUS OPFMOINS OPMOINS
%left OPDIV OPFDIV OPMULT OPFMULT


%right OPOR
%right OPAND

%nonassoc UNIT
%nonassoc Unit

%%

prog:
  | xs =  list(statement) EOF { ast_of_list xs }
  ;
statement:
  | e = expr EOL+ { e } 
  | e = expr EOF { e }
expr: 
  | LET x = ID l = list(arg) EQUALS  EOL* xs =list(seq) EOL* SEMICOLON  { Let (x,  l, ast_of_list xs) } 
  | IF e = expr THEN EOL* ets = list(seq)  EOL* ELSE EOL* efs = list(seq)  EOL* ENDIF { If(e, ast_of_list ets, ast_of_list efs) }
  | FOR x=ID EQUALS vmin=expr TO vmax=expr DO EOL* xs =list(seq) EOL* DONE {For(x,vmin,vmax,ast_of_list xs)}
  | LEFTBR l = separated_list(SEMICOLON, listel) RIGHTBR { Liste(l) }
  | e1 = expr op = binop e2 = expr %prec Binop { Binop(op, e1, e2) }
  | e = expr e2 = expr  %prec Application {App(e,e2)}
  | LEFTPAR e = expr RIGHTPAR { Paren e }
  | d = destination ASSIGN e = expr { Assign(d,e) }
  | e = condition { e }
  | BEGIN EOL* xs = list(seq) EOL* END { ast_of_list xs }
  | DIST x = ID EQUALS e1 = expr SEMICOLON  { Dist (x, e1) } 
  | PPL_SAMPLE e = expr     { Proba (Sample, e) }
  | PPL_ASSUME e = expr     { Proba (Assume, e) }
  | PPL_INFER e = expr   { Proba (Infer, e) }
  | PPL_OBSERVE LEFTPAR e1 = expr RIGHTPAR  { Observe (e1, Nop) } 
  | PPL_OBSERVE LEFTPAR e1 = expr PIPE e2 = expr RIGHTPAR { Observe (e1, e2) } 
  | PPL_FACTOR e = expr   { Proba (Factor, e) }
  | PPL_METHOD s = ID  EOL { Method(s) }
  | PRINT e = printable  { e }
  | UNIT %prec Unit { Unit }
  | x = ID POINT LEFTPAR e = expr RIGHTPAR { Arr(x, e) }
  | c = constant { c }
  | x = ID { Var x }
  ;

constant:
  | i = INT { Int i }
  | f = REAL { Real f }
  | OPMOINS i = INT %prec MoinsSeul { Int (-i) }
  | OPMOINS f = REAL %prec MoinsSeul  { Real (-.f) }
  | s = STRING { String s }
  ;

printable:
  | s = STRING l = list(listel) { App( Print(Text, s), List.fold_right (fun elem acc -> App(elem,acc)) l Nop) } 
  | x = ID { Print(Distrib, x) }
  ;

destination:(*type lvalue*)
  | x = ID { Var x }
  | x = ID POINT LEFTPAR e = expr RIGHTPAR { Arr(x, e) }
  ;

arg:
  |x = ID { Var(x) }
  |UNIT {Unit}
  ;

listel:
  // |e = expr SEMICOLON {e}
  |e = expr {e}
  ;

seq:
  // |e = expr SEMICOLON {e}
  |e = expr EOL* {e}
  ;

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
  ;

condition:
  |e1 = expr OPLT e2 = expr { Cond(LT, e1, e2) }
  |e1 = expr OPLEQ e2 = expr { Cond(Leq, e1, e2) }
  |e1 = expr OPGT e2 = expr { Cond(LT, e2, e1) }
  |e1 = expr OPGEQ e2 = expr { Cond(Leq, e2, e1) }
  |e1 = expr EQUALS e2 = expr { Cond(Eq, e1, e2) }
  ;
  