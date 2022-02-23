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
  | Arr of string * expr
  | Real of float
  | Paren of expr
  | Unit
  | Assign of expr * expr (*Var ou Arr, et valeur*)
  | Liste of expr list
  | For of string * expr * expr * expr (*id, début, fin, corps de la boucle*)
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
 


val ast_of_list : expr list -> expr