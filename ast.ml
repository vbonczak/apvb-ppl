type bop =
  | Add
  | Mult
  | Leq

type proba =
  | Assume
  | Infer
  | Observe
  | Factor
  | Sample

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | Proba of proba * expr
