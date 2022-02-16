open Interp
open Lang
open Ast
open Sys
open Array
open Printf

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
    |Var(s) -> print out "Var";print out s
    |Int(i) -> print out "Int"; print_int out i
    |Dist(s, e) -> print out ("Soit la distribution " ^ s ^ " :=\n"); print_ast_indent out c e;
    |StdCaml(s) -> print out ("Code OCaml :\n"^s^"\n")
    |Proba(_, e) -> print out "Construction proba sur :\n";print_ast_indent out (c+1) e;
    |Seq(e1,e2) -> print out "SEQ"; print_ast_indent out c e1; print_ret out; print_ast_indent out c e2;
    |Observe(e1, e2) -> print out "Construction proba Observe sur :\n";print_ast_indent out (c+1) e1;print out "\net\n";print_ast_indent out (c+1) e2;
    |Method(m) -> print out @@ "Nous utilisons ci-dessous la méthode '"^m^"'"
    |Print(t, s) -> (
      match t with 
      |Distrib -> print out @@ "Visualisation de la distribution"^s;print_ret out
      |Text -> printf "Affichage de \"%s\"\n" s
    )
    |Nop -> ()
  in
  print_ast_indent out 0 e

let fff e =
  print_ast !output e
;;

let () =
 
 
    (*print_ast !output @@ parse_channel !input*)
    
  if length argv > 1 then begin
    output:=open_out argv.(1);
    try 
      if length argv > 2 then  input:=open_in argv.(2);
      (*print_ast !output @@ parse_channel ic*)
      precompile test_funny_bern_ast !output;
      compile argv.(1)
        (*let line = input_line ic in
          print_endline line;      
          flush !output;            
          close_in ic      *)        
    with e ->                    
        (*close_in_noerr ic;        *)
        raise e    
  end
  else  printf "Please enter a destination file to compile to.\nUsage: %s <OUTPUT> [<INPUT>]" executable_name
  ;;  
    
  
    
