{
open Parser

let caml = ref true

let err lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  failwith @@ msg^(string_of_int(p.Lexing.pos_lnum))
           
           
let string_buff = Buffer.create 10000
let reset_caml_buffer () = Buffer.clear string_buff
let store_caml_chars s = Buffer.add_string string_buff s
let get_stored_caml () = Buffer.contents string_buff
let store_caml_char c = Buffer.add_char string_buff c

let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }

}

let white = [' ' '\t']+
let newline = ['\n' '\r' ';']
let digit = ['0'-'9']
let int = '-'? digit+
let real = '-'? digit+'.'digit*
let letter = ['a'-'z' 'A'-'Z']
let id = (letter) (letter|digit|'_'|'.')*

rule read =
  parse
    white+ { read lexbuf }
  | newline { (*incr_loc lexbuf 0;*) EOL }
  | "|" { PIPE } 
  | "(" { LEFTPAR }
  | ")" { RIGHTPAR }
  | "[" { LEFTBR }
  | "]" { RIGHTBR }
  | ";" { SEMICOLON }
  | "<=" { OPLEQ }
  | "=>" { OPGEQ }
  | "<" { OPLT }
  | ">" { OPGT }
  | "=" { EQUALS }

  | "+." { OPFPLUS }
  | "+" { OPPLUS }
  | "-" { OPMOINS }
  | "-." { OPFMOINS }

  | "*." { OPFMULT }
  | "*" { OPMULT }
  | "/." { OPFDIV }
  | "/" { OPDIV }

  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "begin" { BEGIN }
  | "end" { END }
  | "else" { ELSE }
  | "dist" { DIST }
  | "sample" {PPL_SAMPLE}
  | "assume" {PPL_ASSUME}
  | "infer" {PPL_INFER}
  | "observe" {PPL_OBSERVE}
  | "factor" {PPL_FACTOR}
  | "method" {PPL_METHOD}
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | real { REAL (float_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF } 
  | _ { CAML (Lexing.lexeme lexbuf) }
  
  (*  reset_caml_buffer();
    stdcaml lexbuf;
    CAML(get_stored_caml())
  | _
	{
		err lexbuf ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf))
	}*)
 