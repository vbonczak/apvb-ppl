type bop =
| BAnd
| BOr
| Add
| Sub
| Mult
| Div 
| AddF
| SubF
| MultF
| DivF

 

type cond =
  | LT
  | Leq
  | Eq

type proba =
  | Assume
  | Infer
  | Factor
  | Sample

type printable =
  | Distrib
  | Text

type expr = 
  | Var of string
  | Int of int 
  | Real of float
  | Unit
  | Liste of expr list
  | Binop of bop * expr * expr
  | Cond of cond * expr * expr
  | Dist of string * expr
  | App of expr * expr
  | Let of string * expr list * expr  (*id, arguments, contenu*)
  | If of expr * expr * expr
  | Proba of proba * expr
  | Observe of expr * expr
  | Seq of expr * expr
  | Method of string
  | String of string
  | Print of printable * string (*Sortie de la distribution spécifiée par son nom, ou alors juste une chaîne*)
  | Nop

let rec ast_of_list = function
  |[] -> Nop
  |t::q -> Seq(t, ast_of_list q)
  ;;