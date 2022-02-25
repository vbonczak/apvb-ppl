open Ast

val parse_string : string -> expr
val parse_channel : in_channel -> expr
val precompile : expr -> out_channel -> unit
val compile : string -> unit 
