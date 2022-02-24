open Ast
open Printf
open String

open Option

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
  print out "\n";;
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
|"MetroSingle" -> "MH_Single"
|"MetroMulti" -> "MH_MS"
|s -> ignore (failwith ("valeur "^s^" invalide ici")); ""
;;




let snippet_print_gen t s =
  match t with
  |Distrib -> sprintf "let { values; probs; _ } = Option.get %s.support in
Array.iteri (fun i x -> Format.printf \"%%d %%f@.\" x probs.(i)) values;" s
  |Text -> sprintf "Format.printf \"%s\"" s
;;

(*MetroSingle*)
type location = LDecl  (* Which declaration in a sequence of instructions without for or if *)
  | LIf of bool * location
  | LFor of int * location
  (* Describes where a variable is located in the code so as to distinguish different 
  instructions of the form "let x = sample ..." *)

let rec loc_pushback loc (*toloc*) = function 
| LDecl -> loc
| LIf(b, l) -> LIf(b, loc_pushback loc l)
| LFor(n, l) -> LFor(n, loc_pushback loc l)
;;

type vartype =
| SetMetroComp of (float->float->bool) (* fonction de comparaison pour Métropolis *)
| SetInferSamples of int
| OtherSetting of string 
| Distribution (* Une distribution introduite par dist *)
(* à étoffer pour vérifier plus de choses de notre côté *)
(*Spécialités pour MétroSingle*)
| VarInfo of location * string list (*Location ci dessus et liste de variables qui en dépendent*)
| Scope of string list (*Les variables dont on dépend à ce point du programme*)
| Loc of location option (*Notre emplacement actuel, none = toplevel*)
;;
 

let builtin_dists = ["bernoulli";"normal";"uniform";"binomial"];;

let list_of_scope = function | Scope l -> l | _ -> [] ;;
let locopt_of_loc = function | Loc l -> l | _ -> None ;;

let vartype_VarInfo_loc = function | VarInfo(l,_)->l | _ ->ignore (failwith ("Mauvais type d'information dans l'environnement (VarInfo attendu).")); LDecl ;;

let vartype_VarInfo_deps = function | VarInfo(_,d)->d | _ ->
  ignore (failwith ("Mauvais type d'information dans l'environnement (VarInfo attendu).")); [] ;;

let dist_in_env env name = 
  match Hashtbl.find_opt env name with
  |Some x -> (match x with Distribution -> true | _ -> false)
  |None -> ignore (failwith ("La distribution "^name^" est introuvable.")); false
;;

let rec is_dist env (*expr*) =  function
  |App(a, _) -> is_dist env a
  |Paren(e) -> is_dist env e
  |Var x -> (List.mem x builtin_dists) || (dist_in_env env x)
  |_ -> false
;;
 

let is_smetropolis env = 
  Hashtbl.find env "Method" = OtherSetting("MetroSingle")
;;

let rec find_vars = function
|Var x -> [x]
|App (e1,e2) -> (find_vars e1) @ (find_vars e2)
|Paren e -> find_vars e
|Let(_,_,e) -> find_vars e
|If(e, v, f)  -> (find_vars e) @(find_vars v)@(find_vars f)
|_ -> []
;;

let rec print_loc = function
| LDecl -> "LDecl"
| LIf(b, l) ->sprintf "LIf(%b , %s)" b (print_loc l)
| LFor(n, l) -> sprintf "LFor(%n , %s)" n (print_loc l)
;;

(* Production d'un fichier OCaml à partir de notre langage *)
let precompile (e:expr) out  = 
  let rec prodcode out env = function
  |Var(v) -> print out v
  |Int(i) -> fprintf out "%d" i
  |Real(i) -> fprintf out "%f" i
  |Arr(id, e) -> fprintf out "%s.(" id; prodcode out env  e; print out ")"
  |Unit -> print out "()"
  |Liste(l) -> print out "["; List.iteri (fun idx e -> (if idx > 0 then print out "; "); prodcode out env  e; ) l; print out "]"
  |Paren e -> print out "("; prodcode out env  e; print out ")" 
  |App(a,b) -> prodcode out env  a; print out "  "; prodcode out env  b;print out "  ";
  |Binop(op, e1, e2) -> manage_binop out env e1 e2 op
  |Cond(c, e1, e2) -> manage_condition out env e1 e2 c
  (*Pour métropolis*)
  |Let(x,[], Proba(Sample, e)) when is_smetropolis env ->  
    let infos = sprintf "let %s = sample \"%s\" " x x in 
    let curloc = locopt_of_loc (Hashtbl.find env "Loc") in
    Hashtbl.add env x (VarInfo(
        (if is_none curloc  
          then (*Toplevel*) LDecl 
          else (*Imbriqué*) loc_pushback LDecl (Option.get curloc)
        )
      , [])); (*Personne ne dépend de x pour l'instant*)
    fprintf out "(* LOC(%s) = %s *)\n" x (print_loc (vartype_VarInfo_loc (Hashtbl.find env x)));
    (*Par contre, x dépend des variables dans notre scope ? TODO rechercher auxquelles on fait référence*)

    List.iter   (*Hashtbl.find env "Scope"*)
        (fun var -> let info =  Hashtbl.find env var in (*Plante si une variable n'est pas dans la portée! TODO gestion de l'erreur*)
        Hashtbl.replace env var (VarInfo(vartype_VarInfo_loc info , x::(vartype_VarInfo_deps info))) )
        (find_vars e); (*ajout de x dans les variables dépendant de var*)

    let cur_scope = list_of_scope (Hashtbl.find env "Scope") in (*Liste du contexte*)
    Hashtbl.replace env "Scope" (x::cur_scope); (*Ajout de x dans le contexte actuel*)
    prodcode out env (e);
    (*Le let ne se "referme" jamais de son initiative (voir le cas du if à ce sujet)*)

    print out " in\n"

  |If(Proba(Sample, e), vrai, faux) when is_smetropolis env -> 
    print out "if "; 
    prodcode out env  e; 
    (*Sauvegarde de notre scope*)
    let cur_scope = Hashtbl.find env  "Scope" in
    (*Contexte location actuel*)
    let curloc = locopt_of_loc (Hashtbl.find env "Loc") in

    print out "then begin\n"; (*true*)
    let loc_true = (if is_none curloc  
      then (*Toplevel*) LIf(true, LDecl) 
      else (*Imbriqué*) loc_pushback LIf(true, LDecl)  (Option.get curloc)
    ) in
    Hashtbl.replace env "Loc" loc_true; (*mise à jour de la loc actuelle*)
    prodcode out env v;

    print out "\n end\n else begin\n"; (*false*)
    let loc_false = (if is_none curloc  
      then (*Toplevel*) LIf(false, LDecl) 
      else (*Imbriqué*) loc_pushback LIf(false, LDecl)  (Option.get curloc)
    ) in
    Hashtbl.replace env "Loc" loc_false; (*mise à jour de la loc actuelle, autre choix*)
    prodcode out env  f; print out "\nend\n"
    
    (*Rétablissement du scope avant le if*)
    Hashtbl.replace env  "Scope" cur_scope;
    (*et de la loc*)
    Hashtbl.replace env "Loc" cur_loc;
  |For(x,vmin,vmax,body)  when is_smetropolis env -> fprintf out "for %s = " x; prodcode out env  vmin; print out " to "; prodcode out env  vmax; print out " do\n";
    (*Corps de boucle TOTO single metro*) prodcode out env  body;
                    print out "\ndone;\n";

  (*les autres*)
  |Let(x,l,e) -> fprintf out "let %s %s = " x (List.fold_left (
    fun a b -> a^" "^(match b with 
                |Var(x)-> x
                |Unit->" () "
                |_-> failwith "Erreur dans un let, l'objet n'est ni un identificateur ni ()."
              ) 
            ) "" l) ;
    prodcode out env  e; 
    (match l with
    |[] -> if x <> "_" then print out "in\n"
    |_ when is_smetropolis env (*Une fonction : on donne les variantes first_f et resample_f qui vont retourner 
    le tableau des variables associées à leur emplacement et les variables qui en dépendent*) ->
      fprintf out "\nlet first_%s = " x; smetro_generate_first_variant out env e; print out ";;\n"

      fprintf out "\nlet resample_%s = " x; smetro_generate_resample_variant out env e; print out ";;\n"
    |_ -> print out "\n"; )
  |If(e,v,f) -> print out "if "; prodcode out env  e; print out "then begin\n";prodcode out env  v;
                print out "\n end\n else begin\n"; prodcode out env  f;print out "\nend\n"
  |For(x,vmin,vmax,body) -> fprintf out "for %s = " x; prodcode out env  vmin; print out " to "; prodcode out env  vmax; print out " do\n";
  (*Corps de boucle*) prodcode out env  body;
                  print out "\ndone;\n";
  |Assign(d, e) -> manage_assign out env d e
  |Dist(s, e) -> fprintf out "let %s = " s; 
                      Hashtbl.add env s Distribution; (* Le type de s est distribution *)
                      prodcode out env  e; print out "in\n"
  |String(s) -> fprintf out "\"%s\"" s
  |Proba(p, e) ->  gen_prob_cstr e env p 
  |Seq(e1,e2) -> prodcode out env  e1; (match e1 with
                 |App(_,_)-> print out ";\n"
                 |_ -> print_ret out);prodcode out env  e2
  |Observe(e1, e2) -> print out "observe ";prodcode out env  e1; print out "  "; prodcode out env  e2; print out " "
  |Method(m) -> fprintf out "open %s\n"  (module_of_infer_method m); Hashtbl.add env "Method" (OtherSetting m)
  |Print(t, s) ->  print out @@ (snippet_print_gen t s) ^ "\n"
  |Nop -> ()


  and manage_condition out env e1 e2 c =   prodcode out env  e1; (match c with
  | LT  -> print out " < " 
  | Leq -> print out " <= " 
  | Eq  -> print out " = ")
    ; prodcode out env  e2


  and manage_binop out env e1 e2 c =   prodcode out env  e1; (match c with
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
    ; prodcode out env  e2    


  and manage_assign out env d e =
    (match d with
    |Arr (id, i) -> fprintf  out "%s.(" id;  prodcode out env  i; print out ") <- "
    |Var x -> fprintf out "%s := " x
    |_ -> failwith "Assignement invalide."
    );
    prodcode out env  e
    (*Génération de la ligne de code pour la construction probabiliste*)


  and  gen_prob_cstr expr env p= 
  (match p with
    | Assume -> print out "assume " 
    | Infer -> print out "infer 10000 " 
    | Factor -> print out "factor "
    | Sample -> print out "sample "; if not (is_dist env expr) then failwith "Sample utilisé avec un objet qui n'est pas une distribution" (*La suite immédiate doit être une distribution*)
    ); prodcode out env  expr;  print out ";"



  in



  (* Tableau des variables -> type *)
  let ic = open_in "templates/general2.mlt" in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  fprintf out "%s\n(*Fin de la partie d'entête*)\n" s;
  let env = Hashtbl.create 10 in
  Hashtbl.add env "File" (OtherSetting "general2");
  Hashtbl.add env "Metro" (SetMetroComp((<=)));
  Hashtbl.add env "Infer" (SetInferSamples(1000));
  Hashtbl.add env "Method" (OtherSetting "undef");
  Hashtbl.add env "Scope" (Scope []);
  Hashtbl.add env "Loc" (Loc None);
  prodcode out env  e
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
