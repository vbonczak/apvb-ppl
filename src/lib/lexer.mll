{
open Parser

let err lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  failwith @@ msg^(string_of_int(p.Lexing.pos_lnum))
                         


let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }

}

let white = [' ' '\t']+
let digit = ['0'-'9']
let float = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "<=" { LEQ }
  | "*" { TIMES }
  | "+" { PLUS }
  | "|" { PIPE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "let" { LET }
  | "fun" { FUN }
  | "->" { TO }
  | "=" { EQUALS }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "end" { END }
  | ";" { END_LINE }
  | "sample" {PPL_SAMPLE}
  | "assume" {PPL_ASSUME}
  | "infer" {PPL_INFER}
  | "observe" {PPL_OBSERVE}
  | "factor" {PPL_FACTOR}
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | '\n' { incr_loc lexbuf 0; read lexbuf }
  | _
	{
		err lexbuf ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf))
	}