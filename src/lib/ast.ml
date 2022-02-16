(*type bop =
  | Add
  | Mult
  | Leq*)

type proba =
  | Assume
  | Infer
  | Factor
  | Sample

type expr = 
  | Var of string
  | Int of int 
  | Dist of string * expr
  | Proba of proba * expr
  | Observe of expr * expr
  | Seq of expr * expr
  | StdCaml of string
  | Nop