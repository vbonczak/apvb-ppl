open Ast
open Printf
open String


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

let print out =
  Printf.fprintf out "%s"
;;

let print_ret out =
  print out "\n"

let test_funny_bern_ast : expr = 
  ast_of_list [
    Method("Rejection");
  (*avec des retours direct dans la chaîne pour l'instant*)
    StdCaml("let funny_bernoulli prob () = ");
    StdCaml("let a = "); 
    Proba(Sample, StdCaml("(bernoulli ~p:0.5) in"));
    StdCaml("let b = "); Proba(Sample, StdCaml("(bernoulli ~p:0.5) in"));
    StdCaml("let c = "); Proba(Sample, StdCaml("(bernoulli ~p:0.5) in"));
  Proba(Assume, StdCaml("a = 1 || b = 1"));
  StdCaml("a+b+c");
  StdCaml("let _ =");
  Print(Text, "@.-- Funny Bernoulli, Basic Rejection Sampling --@.");
  Dist("d", Proba(Infer, StdCaml("funny_bernoulli ()")));
  Print(Distrib, "d")
  ]    ;;

let dependencies = ["owl"];;

let module_of_infer_method = function
|"Rejection" -> "Rejection_sampling"
|"Importance" -> "Importance_sampling"
|s -> "Array (*valeur "^s^" invalide ici*)" 
;;



let gen_assume = function
| StdCaml(s) -> "assume prob ("^s^");"
| _ -> "(*Assume invalide*)"
;;

let gen_infer = function
| StdCaml(s) -> "infer "^s
| _ -> "(*Infer invalide*)"
;;

let gen_factor = function
| StdCaml(s) -> "factor "^s
| _ -> "(*Factor invalide*)"
;;

let gen_sample = function
| StdCaml(s) -> "sample prob "^s
| _ -> "(*Sample invalide*)"
;;

(*Génération de la ligne de code pour la construction probabiliste*)
let gen_prob_cstr expr = function
| Assume -> gen_assume expr
| Infer -> gen_infer expr
| Factor -> gen_factor expr
| Sample -> gen_sample expr
;;

let snippet_print_gen t s =
  match t with
  |Distrib -> sprintf "let { values; probs; _ } = get_support ~shrink:true %s in
Array.iteri (fun i x -> Format.printf \"%%d %%f@.\" x probs.(i)) values;" s
  |Text -> sprintf "Format.printf \"%s\";" s
;;

(* Production d'un fichier OCaml à partir de notre langage *)
let precompile (e:expr) out = 
  let rec prodcode out = function
  |Var(_) -> ()
  |Int(_) -> ()
  |Dist(s, e) -> fprintf out "let %s = " s; prodcode out e; print out "in\n"
  |StdCaml(s) -> fprintf out "%s\n" s
  |Proba(p, e) ->  fprintf out "%s\n" @@ gen_prob_cstr e p 
  |Seq(e1,e2) -> prodcode out e1;print_ret out; prodcode out e2
  |Observe(e1, e2) -> print out "\n(*OBSERVE";  prodcode out e1; prodcode out e2; print out "*)\n"
  |Method(m) -> fprintf out "open %s\n"  (module_of_infer_method m)
  |Print(t, s) ->  print out @@ (snippet_print_gen t s) ^ "\n"
  |Nop -> print_ret out
  in
  let ic = open_in "templates/general.mlt" in
  let s = really_input_string ic (in_channel_length ic) in
  fprintf out "%s\n(*Fin de la partie d'entête*)\n" s;
  prodcode out e
;;

let compile path =
  (*Les dépendances sous la forme attendue par ocamlc à savoir dep1,dep2,...*)
  let deps = List.fold_left (fun a b -> (if length a > 0 then (a ^ ",") else "") ^ b) "" dependencies in
  
  let base_name_end =  (try rindex path '.'
                       with Not_found -> length path) in
  let base_name = sub path 0 base_name_end in 
  let mlfile = base_name ^ ".ml" in
  (*let ich = open_in path in*)
    let ast = (* parse_channel ich*) test_funny_bern_ast in
    (*close_in ich*)
    let och = open_out mlfile in
    precompile ast och;
    printf "Fichier %s précompilé dans %s\n" path mlfile;
    close_out och;
    (*Compilation du fichier créé juste avant*)
    let exefile = base_name^".out" in
    let cmd = sprintf  "ocamlfind ocamlc -o \"%s\" -package %s -linkpkg \"%s\"" exefile deps mlfile in
    printf "$ > %s" cmd;
    match Sys.command cmd with
    |0 -> printf "Sortie : %s (dépendant de %s)\n" exefile deps
    |n -> printf "Erreur %d\n" n;
    
  
  (*  *)
;;
 
  (* Sys.command "ocamlfind ocamlc -o test.out -linkpkg %s test.ml" *)

