%{
open Ast
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token LEQ
%token TIMES
%token PLUS
%token LPAREN
%token RPAREN
%token LET
%token FUN
%token TO
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE

%token PPL_SAMPLE
%token PPL_ASSUME
%token PPL_INFER
%token PPL_OBSERVE
%token PPL_FACTOR

%token END_LINE

%token END
%token EOF

%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left TIMES
%left END_LINE

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;
  
expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If (e1, e2, e3) }
  | FUN; fun_var = ID; TO; fun_body = expr; END { Fun (fun_var, fun_body) }
  | LPAREN; e=expr; RPAREN {e}
  | PPL_SAMPLE; e = expr  ; END_LINE { Proba (Sample, e) }
  | PPL_ASSUME; e = expr  ; END_LINE{ Proba (Assume, e) }
  | PPL_INFER; e = expr; END_LINE { Proba (Infer, e) }
  | PPL_OBSERVE; e = expr; END_LINE { Proba (Observe, e) }
  | PPL_FACTOR; e = expr ; END_LINE{ Proba (Factor, e) }
  | e = expr; END_LINE; e2 = expr {Seq (e, e2) }
  ;
