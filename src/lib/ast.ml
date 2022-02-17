(*type bop =
  | Add
  | Mult
  | Leq*)

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
  | Dist of string * expr
  | Let of string * expr list * expr  (*id, arguments, contenu*)
  | Proba of proba * expr
  | Observe of expr * expr
  | Seq of expr * expr
  | Method of string
  | StdCaml of string
  | Print of printable * string (*Sortie de la distribution spécifiée par son nom, ou alors juste une chaîne*)
  | Nop

let rec ast_of_list = function
  |[] -> Nop
  |t::q -> Seq(t, ast_of_list q)
  ;;