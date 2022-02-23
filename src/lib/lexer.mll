{
open Parser
 open Lexing
open Printf
let err lexbuf msg =
  let p = lexeme_start_p lexbuf in
  failwith @@ sprintf "%s à la ligne %d" msg p.Lexing.pos_lnum
           
let string_buff = Buffer.create 10000
let reset_string_buffer () = Buffer.clear string_buff
let store_string_chars s = Buffer.add_string string_buff s
let get_stored_string () = Buffer.contents string_buff
let store_string_char c = Buffer.add_char string_buff c
 
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

} 

let white = [' ' '\t']+
let newline = ['\n' '\r']
let digit = ['0'-'9']
let int =  digit+
let real = digit+'.'digit*
let letter = ['a'-'z' 'A'-'Z']
let id = (letter|'_'|'!') (  (letter|digit|'_'|'.')+ (letter|digit|'_'|'!') | (letter|digit|'_')*   )


rule read =
  parse
    white+ { read lexbuf }
  | newline { next_line lexbuf; EOL }
  | "||" { OPOR } 
  | "&&" { OPAND } 
  | "|" { PIPE } 
  | "()" {UNIT}
  | ":=" { ASSIGN }
  | "(" { LEFTPAR }
  | ")" { RIGHTPAR }
  | "[" { LEFTBR }
  | "]" { RIGHTBR }
  | ";" { SEMICOLON }
  | "," { COLON }
  | "<=" { OPLEQ }
  | "=>" { OPGEQ }
  | "<" { OPLT }
  | ">" { OPGT }
  | "=" { EQUALS }
  | "." { POINT }
  | "+." { OPFPLUS }
  | "+" { OPPLUS }
  | "-." { OPFMOINS }
  | "-" { OPMOINS }

  | "*." { OPFMULT }
  | "*" { OPMULT }
  | "/." { OPFDIV }
  | "/" { OPDIV }
  | '"'    { string (Buffer.create 1024) lexbuf }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }


  | "for"{FOR}
  | "to"{TO}
  | "do"{DO}
  | "done"{DONE}

  | "print" {PRINT}
  | "endif" { ENDIF }
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
  | _ {err lexbuf "Illegal character: " }
and string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' 'n'  { Buffer.add_char buf '\n'; string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      string buf lexbuf
    }
  | _ { err lexbuf "Illegal string character" }
  | eof {  err lexbuf "String is not terminated" }
  (*  reset_caml_buffer();
    stdcaml lexbuf;
    CAML(get_stored_caml())
  | _
	{
		err lexbuf ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf))
	}*)
 