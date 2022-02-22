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

let print out =
  Printf.fprintf out "%s"
;;

let print_ret out =
  print out "\n"
(*
let test_funny_bern_ast : expr = 
  ast_of_list [
    Method("Rejection");
  (*avec des retours direct dans la chaîne pour l'instant*)
    Let("funny_bernoulli", );
    StdCaml("let a = "); 
    Proba(Sample, StdCaml("(bernoulli 0.5) in"));
    StdCaml("let b = "); Proba(Sample, StdCaml("(bernoulli 0.5) in"));
    StdCaml("let c = "); Proba(Sample, StdCaml("(bernoulli 0.5) in"));
  Proba(Assume, StdCaml("a = 1 || b = 1"));
  StdCaml("a+b+c");
  StdCaml("let _ =\nRandom.self_init();");
  Print(Text, "@.-- Funny Bernoulli, Basic Rejection Sampling --@.");
  Dist("d", Proba(Infer, StdCaml("funny_bernoulli")));
  Print(Distrib, "d")
  ]    ;;*)

  let test_funny_bern_ast : expr = Nop;;

let dependencies = [];;

let module_of_infer_method = function
|"Rejection" -> "Rejection_sampling"
|"Importance" -> "Importance_sampling"
|s -> "Array (*valeur "^s^" invalide ici*)" 
;;




let snippet_print_gen t s =
  match t with
  |Distrib -> sprintf "let { values; probs; _ } = Option.get %s.support in
Array.iteri (fun i x -> Format.printf \"%%d %%f@.\" x probs.(i)) values;" s
  |Text -> sprintf "Format.printf \"%s\";" s
;;

(* Production d'un fichier OCaml à partir de notre langage *)
let precompile (e:expr) out = 
  let rec prodcode out = function
  |Var(v) -> print out v
  |Int(i) -> fprintf out "%d" i
  |Real(i) -> fprintf out "%f" i
  |Unit -> print out "()"
  |Liste(l) -> print out "["; List.iter (fun e -> prodcode out e; print out "; ") l; print out "]"
  |App(a,b) -> prodcode out a; print out "("; prodcode out b;print out ")";
  |Binop(op, e1, e2) -> manage_binop out e1 e2 op
  |Cond(c, e1, e2) -> manage_condition out e1 e2 c
  |Let(x,l,e) -> fprintf out "let %s %s = " x (List.fold_left (
    fun a b -> a^(match b with 
                |Var(x)-> x
                |Unit->"()"
                |_-> "NON!"
              ) 
            ) "" l) ;
    prodcode out e; 
    (match l with
    |[] -> if x <> "_" then print out "in\n"
    |_ -> print out "\n")
  |If(e,v,f) -> print out "if "; prodcode out e; print out "then begin\n";prodcode out v;
                print out "\n end\n else begin\n"; prodcode out f;print out "\nend\n"
  |Dist(s, e) -> fprintf out "let %s = " s; prodcode out e; print out "in\n"
  |String(s) -> fprintf out "\"%s\"" s
  |Proba(p, e) ->  gen_prob_cstr e p 
  |Seq(e1,e2) -> prodcode out e1;print_ret out ; prodcode out e2
  |Observe(e1, e2) -> print out "\n(*OBSERVE";  prodcode out e1; prodcode out e2; print out "*)\n"
  |Method(m) -> fprintf out "open %s\n"  (module_of_infer_method m)
  |Print(t, s) ->  print out @@ (snippet_print_gen t s) ^ "\n"
  |Nop -> print_ret out
  and manage_condition out e1 e2 c =   prodcode out e1; (match c with
  | LT  -> print out " < " 
  | Leq -> print out " <= " 
  | Eq  -> print out " = ")
    ; prodcode out e2
  and manage_binop out e1 e2 c =   prodcode out e1; (match c with
  | BAnd -> print out " && " 
  | BOr ->print out " || " 
  | Add -> print out " + " 
  | Sub -> print out " - " 
  | Mult -> print out " * "
  |  Div  ->  print out " / "
  | AddF -> print out " +. " 
  | SubF -> print out " -. " 
  | MultF -> print out " *. "
  |  DivF  ->  print out " /. ")
    ; prodcode out e2    
    (*Génération de la ligne de code pour la construction probabiliste*)
  and  gen_prob_cstr expr p= 
  (match p with
    | Assume -> print out "assume ("  
    | Infer -> print out "infer 10000 ("  
    | Factor -> print out "factor ("
    | Sample -> print out "sample ("
    ); prodcode out expr;  print out ");"
  in

  let ic = open_in "templates/general2.mlt" in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  fprintf out "%s\n(*Fin de la partie d'entête*)\n" s;
  prodcode out e
;;

let compile path =
  let base_name_end =  (try rindex path '.' with Not_found -> length path) in
  let base_name = sub path 0 base_name_end in 
  (*Précompilation de notre langage dans le fichier truc.mlppl vers truc.ml*)
  let mlfile = base_name ^ ".ml" in   
  (*Les dépendances sous la forme attendue par ocamlc à savoir dep1,dep2,...*)
  let deps = List.fold_left (fun a b -> (if length a > 0 then (a ^ ",") else "") ^ b) "" dependencies in
  let ich = open_in path in
  let ast = parse_channel ich (*test_funny_bern_ast *) in
  close_in ich;
  let och = open_out mlfile in
  precompile ast och;
  printf "Fichier %s précompilé dans %s\n" path mlfile;
  close_out och;
  (*Compilation du fichier créé juste avant*)
  let exefile = base_name^".out" in
  let cmd = sprintf  "ocamlfind ocamlc -o \"%s\" %s \"%s\"" exefile (match length deps with
                                                                    |0 -> ""
                                                                    |_ -> sprintf "-package %s -linkpkg" deps
                                                                    )                                         mlfile in
  printf "$ > %s" cmd;
  match Sys.command cmd with
  |0 -> printf "Sortie : %s (dépendant de %s)\n" exefile deps
  |n -> printf "Erreur %d\n" n;
;;
