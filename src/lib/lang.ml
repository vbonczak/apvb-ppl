open Ast
open Printf

let parse_string (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let parse_channel (c : in_channel) : expr =
  let lexbuf = Lexing.from_channel c  in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let rec ast_of_list = function
|[] -> Nop
|t::q -> Seq(t, ast_of_list q)
;;


let test_funny_bern_ast : expr = 
  ast_of_list [
  (*avec des retours direct dans la chaîne pour l'instant*)
    StdCaml("let funny_bernoulli () = 
    let a = sample (bernoulli ~p:0.5) in
    let b = sample (bernoulli ~p:0.5) in
    let c = sample (bernoulli ~p:0.5) in");
  Proba(Assume, StdCaml("a = 1 || b = 1"))
  ]    ;;

let dependencies = ["owl"; "defe"; "nign"];;

(* Production d'un fichier OCaml à partir de notre langage *)
let precompile (e:expr) out = 
  ignore e;
  fprintf out "C%sC" "Coucou"
;;

let compile path =
  let deps = List.fold_left (fun a b -> a ^ " " ^ b) "" dependencies in
  printf "Chemin %s avec \n%s" path deps
;;
  (* Sys.command "ocamlfind ocamlc -o test.out -linkpkg owl test.cmo" *)

