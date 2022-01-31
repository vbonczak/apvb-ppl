open Ast

let parse_string (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let parse_channel (c : in_channel) : expr =
  let lexbuf = Lexing.from_channel c  in
  let ast = Parser.prog Lexer.read lexbuf in
  ast