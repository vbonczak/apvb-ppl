open Interp
open Lang
open Ast
    
let input = ref stdin
let output = ref stdout
    
let basename s =
  try String.sub s 0 (String.rindex s '.')
  with Not_found -> s

let print out =
  Printf.fprintf out "%s"

let print_int out =
  Printf.fprintf out "%d"

let print_indent out n =
  print out @@ (String.make n ' ') ^ "| "
    
let print_ret out =
  Printf.fprintf out "\n"

let print_ast out e =
  let rec print_ast_indent out c e =
    print_indent out c;
    match e with
    |Seq(e1, e2) -> print_ast_indent out (c+1) e1; print_ret out; print_ast_indent out (c+1) e2;print_ret out;
    |Var(s) -> print out s
    |Int(i) -> print_int out i
    |Bool(b) -> print out @@ if b then "true" else "false"
    |Binop(op, e1, e2) -> print out (match op with
      |Add -> "+"
      |Mult -> "*"
      |Leq -> "<="); print_ast_indent out  (c+1) e1; print_ast_indent out (c+1) e2
    |Let(s, e, body) -> print out ("Soit " ^ s ^ " :=\n"); print_ast_indent out c e; print_ast_indent out (c+1) body
    |If(e, e1, e2) -> print out "Si ("; print_ast_indent out 0 e; print out ") Alors :"; print_ast_indent out  (c+1) e1; print out "Sinon :"; print_ast_indent out  (c+1) e2; 
    |Fun(s, body) -> print out ("Fonction "^s^" -> \n"); print_ast_indent out (c+1) body; print_ret out;
    |Proba(_, e) -> print out "Construction proba sur :\n";print_ast_indent out (c+1) e;
    |Observe(e1, e2) -> print out "Construction proba Observe sur :\n";print_ast_indent out (c+1) e1;print out "\net\n";print_ast_indent out (c+1) e2;
  in
  print_ast_indent out 0 e

let fff e =
  print_ast !output e
    
let ffff _ =
  print_ast !output @@ parse_channel !input
    
