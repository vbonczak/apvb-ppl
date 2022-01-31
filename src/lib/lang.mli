open Ast

val parse_string : string -> expr
val parse_channel : in_channel -> expr