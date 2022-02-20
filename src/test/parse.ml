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
    |Let(x,l,e) -> 
                                  (match l with
                                  |[] -> printf "let %s =" x
                                  |_ -> printf  "fonction %s params(%s) = {" x (List.fold_left (fun a b -> (match b with 
                                  |Var(x)-> x^" "
                                  |_-> "autre que var?")^a) "" l));
    print_ast_indent out (c+1) e; print out "}\n"
    |If(e,v,f) -> print out "IF";print_ast_indent out (c+1) e;
    print out "ALORS";print_ast_indent out (c+1) v;print out "SINON";print_ast_indent out (c+1) f;
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
    |Nop -> printf "Nop"
  in
  print_ast_indent out 0 e

let fff e =
  print_ast !output e
;;

let () =
  
 
    (*print_ast !output @@ parse_channel !input*)
    
  if length argv > 1 then begin
  input:=open_in argv.(1);
    try 
      if length argv > 2 then output:=open_out argv.(2);
      let s = really_input_string !input (in_channel_length !input) in
      (* fprintf !output "ce fichier={%s}" s; *)
      close_in !input ;
      print_ast !output @@ parse_string s;
      (*print_ast !output @@ parse_channel ic  <--- menhir a du mal avec la channel*)
        
    with e ->                    
        (*close_in_noerr ic;        *)
        raise e    
  end
  else  printf "Please enter an input file to compile.\nUsage: %s <INPUT> [<OUTPUT>]" executable_name
  ;;  
    
  
    
