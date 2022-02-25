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
type location = LDecl of int(* Which declaration in a sequence of instructions without for or if *)
  | LIf of bool * location
  | LFor of int * location
  (* Describes where a variable is located in the code so as to distinguish different 
  instructions of the form "let x = sample ..." *)

let rec loc_pushback loc (*toloc*) = function 
| LDecl _ -> loc
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

let vartype_VarInfo_loc = function | VarInfo(l,_)->l | _ ->ignore (failwith ("Mauvais type d'information dans l'environnement (VarInfo attendu).")); LDecl (-1);;

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

let rec union a b =
match a,b with 
|[],  l -> l
|l,  [] -> l 
|(t::q), l2 -> if List.mem t l2 then (union q l2) else t::(union q l2)
;;

let (++) = union;;

let rec find_vars = function
|Var x -> [x]
|App (e1,e2) -> (find_vars e1) ++ (find_vars e2)
|Seq (e1,e2) -> (find_vars e1) ++ (find_vars e2)
|Paren e -> find_vars e
|Let(_,_,e) -> find_vars e
|If(e, v, f)  -> (find_vars e) ++(find_vars v)++(find_vars f)
|Binop(_, e1, e2)->(find_vars e1) ++ (find_vars e2)
|Cond (_ , e1, e2)->(find_vars e1) ++ (find_vars e2)
|Proba(_,e)-> find_vars e
|_ -> []
;;

let rec print_loc = function
| LDecl n -> sprintf "LDecl %d" n
| LIf(b, l) -> sprintf "LIf(%b , %s)" b (print_loc l)
| LFor(n, l) -> sprintf "LFor(%n , %s)" n (print_loc l)
;;

(*Cette fonction retourne true si la location that est une sous-location de in_what (imbriquée dedans).
Sert à enlever les variables dont la portée est terminée quand on traite un if ou un for.
  Pas besoin d'identifier quel for ou if cela concerne, ni quelle branche car on les aura enlevé à la fin de leur traitement
    (au else, et au endif pour if).*)
let rec is_in_loc that in_what = 
match that, in_what with
|(LDecl _), (LDecl _) -> true
|(LIf(_, sub_loc)), (LIf(_, in_sub_loc)) -> is_in_loc sub_loc in_sub_loc
|(LFor(_, sub_loc)), (LFor(_, in_sub_loc)) ->  is_in_loc sub_loc in_sub_loc
|_ -> false
;;


let filter_hashtable table loc_when_entering_scope =
  let to_delete = ref [] in (*multiensemble de choses à enlever de la table
   (on peut poper plusieurs "x" par exemple si les deux sont sous cette location)*)
  Hashtbl.iter  (fun k v -> 
    match v with
    (* Loc, liste de deps *)
    |VarInfo(loc, _) when is_in_loc loc loc_when_entering_scope -> to_delete := k::!to_delete
    |_ -> ()
  ) table;
  (* Suppression effective de toutes ces variables *)
  List.iter (fun k -> Hashtbl.remove table k) !to_delete
;;


let add_var_in_table table variable where =
  let l = Hashtbl.find_all table variable in
  (* l est la liste des variables de même nom *)
  Hashtbl.add table variable (VarInfo(
        (if is_none where  
          then (*Toplevel*) LDecl (List.length l)
          else (*Imbriqué*) loc_pushback (LDecl (List.length l)) (Option.get where)
        )
      , [])); (*Personne ne dépend de x pour l'instant*)
    vartype_VarInfo_loc (Hashtbl.find table variable)
;;
  (*Cette valeur va masquer dans la table les autres déclarations précédemment traitées.*)
  
let is_random_variable env var = if Hashtbl.mem env var then 
    begin
      match Hashtbl.find env var with |VarInfo(_) -> true|_->false
    end
  else false
  ;;

let get_random_variables_in env e =
  List.filter (fun v -> is_random_variable env v) (find_vars e);;

  (*Recherche si la variable est une VA, retourne not found sinon*)
let find_random_variable env var =
  let r = Hashtbl.find env var in
  match r with 
  |VarInfo _ -> r
  |_ -> raise Not_found
;;

(*Ajoute x dans les deps de toutes les VA dans to_which_vars*)
let add_to_deps env out x xloc to_which_vars =  
List.iter    
(*f*)    (fun var -> 
          try 
            let info = find_random_variable env var in
                (*On va naturellement remplacer la dernière arrivée*)
                Hashtbl.replace env var (VarInfo(vartype_VarInfo_loc info , x::(vartype_VarInfo_deps info))) ;
                fprintf out "\ndeps__%s := (\"%s\",%s)::!deps_%s;\n" var x (print_loc xloc) var 
          with Not_found -> ())
(*Liste*) to_which_vars
        ;;


(* let print_litteral_list out l = print out "["; List.iteri (fun idx e -> (if idx > 0 then print out "; "); fprintf out "\"%s\"" e ; ) l; print out "]";; *)

(*Place pour chaque variable x = let ... le deps__x et membre_retour_sans_deps__x dans la liste de retour*)
let populate_liste_retour out env expr = 
  List.iter 
    (fun var -> 
      fprintf out "\nliste_retour := (fst membre_retour_sans_deps__%s, deps__%s, snd membre_retour_sans_deps__%s)::!liste_retour;\n" var var var
    )
    (get_random_variables_in  env expr)


(* Production d'un fichier OCaml à partir de notre langage *)
let precompile (e:expr) out  = 
  let rec prodcode out env = function
  |Var(v) -> print out v
  |Int(i) -> fprintf out "%d" i
  |Real(i) -> fprintf out "%f" i
  |Arr(id, e) -> fprintf out "%s.(" id; prodcode out env  e; print out ")"
  |Unit -> print out "()"
  |Liste(l) -> print_list out env l
  |Paren e -> print out "("; prodcode out env  e; print out ")" 
  |App(a,b) -> prodcode out env  a; print out "  "; prodcode out env  b;print out "  ";
  |Binop(op, e1, e2) -> manage_binop out env e1 e2 op
  |Cond(c, e1, e2) -> manage_condition out env e1 e2 c
  (*Pour métropolis*)
  |Let(x,[], Proba(Sample, e)) when is_smetropolis env ->  manage_metro_let out env x e
  (* |For(x,vmin,vmax,body)  when is_smetropolis env -> fprintf out "for %s = " x; prodcode out env  vmin; print out " to "; prodcode out env  vmax; print out " do\n";
    (*Corps de boucle TOTO single metro*) prodcode out env  body;
                  print out "\ndone;\n"; *)
  (* |If( *** Proba(Sample, e) *** , vrai, faux) when is_smetropolis env -> 
    prodcode out env (Seq(Let("if__var",[], Proba(Sample, e)),
      If(Var "if__var")) *)   (*TODO gestion du if avec un sample*)
  |If(e, vrai, faux) when is_smetropolis env -> manage_metro_if out env e vrai faux
  (*les autres*)
  |Let(x,l,e) -> manage_let out env x l e
  |If(e,v,f) -> print out "if "; prodcode out env  e; print out "then begin\n";prodcode out env  v;
                print out "\n end\n else begin\n"; prodcode out env  f;print out "\nend\n"
  |For(x,vmin,vmax,body) -> fprintf out "for %s = " x; prodcode out env  vmin; print out " to "; prodcode out env  vmax; print out " do\n";
  (*Corps de boucle*) prodcode out env  body;
                  print out "\ndone;\n";
  |Assign(d, e) -> manage_assign out env d e
  |Dist(s, e) -> fprintf out "let %s = " s; 
                      Hashtbl.add env s Distribution; (* Le type de s est distribution *)
                      prodcode out env  e; print out " in\n"
  |String(s) -> fprintf out "\"%s\"" s
  |Proba(p, e) ->  gen_prob_cstr e env p 
  |Seq(e1,e2) -> prodcode out env  e1; (match e1 with
                 |App(_,_)-> print out ";\n"
                 |_ -> print_ret out);prodcode out env  e2
  |Observe(e1, e2) -> print out "observe ";prodcode out env  e1; print out "  "; prodcode out env  e2; print out " "
  |Method(m) -> fprintf out "open %s\n"  (module_of_infer_method m); Hashtbl.add env "Method" (OtherSetting m)
  |Print(t, s) ->  print out @@ (snippet_print_gen t s) ^ "\n"
  |Nop -> ()


  (*Fonctions auxiliaires*)


  and manage_let out env x l e = 
    fprintf out "let %s%s %s = " (if is_smetropolis env && List.length l > 0 then "first_" else "") x (List.fold_left (
    fun a b -> a^" "^(match b with 
                |Var(x)-> x
                |Unit->" () "
                |_-> failwith "Erreur dans un let, l'objet n'est ni un identificateur ni ()."
              ) 
            ) "" l) ;
    
    (*si MetroSingle, y a t-il des VA dans l'expression?*)
    if (is_smetropolis env) then begin
      match l with
      |[] -> (*Pas une fonction !*)  
      
        let vas = get_random_variables_in env e in (* Les VA dont dépend x *)
        if (List.length vas > 0) then
          begin
            let curloc = locopt_of_loc (Hashtbl.find env "Loc") in
            let xloc = add_var_in_table env x curloc in (*Ajout de x dans les variables aléatoires donc TODO quelle est sa loc?*)
            add_to_deps env out x xloc vas
          end
      |_ -> (*fonction*)
      print out "\nlet liste_retour = ref [] in (*Liste à renvoyer par first de ((var, loc), deps, valeur samplée)*)\n";
    end;

    prodcode out env  e; 
    match l with
    |[] -> if x <> "_" then print out " in\n"
    |_ when is_smetropolis env -> populate_liste_retour out env e;
      print out "\n, liste_retour\n";(*Une fonction : on donne les variantes first_f et resample_f qui vont retourner 
    le tableau des variables associées à leur emplacement et les variables qui en dépendent*) 
     (* fprintf out "\nlet first_%s = " x; smetro_generate_first_variant out env e; print out ";;\n";

      fprintf out "\nlet resample_%s = " x; smetro_generate_resample_variant out env e; print out ";;\n"*)
    |_ -> print out "\n"

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
    ); 
    prodcode out env  expr;  print out ";"

  and print_list out env l = print out "["; List.iteri (fun idx e -> (if idx > 0 then print out "; "); prodcode out env  e; ) l; print out "]"

  and manage_metro_if out env e vrai faux = 
    print out "if "; 
    prodcode out env  e; 

    (*Les variables aléatoires dont dépend l'expression cond*)
    let vas = get_random_variables_in env e in 
    (*Sauvegarde de notre scope*)
    let cur_scope = list_of_scope (Hashtbl.find env  "Scope") in

    (*Ajout des VA dans e dans le contexte de dépendance actuel*)
    Hashtbl.replace env "Scope" (Scope (vas@cur_scope)); 
    (*location actuel*)
    let curloc = locopt_of_loc (Hashtbl.find env "Loc") in

    print out " then begin\n"; (*true*)
    (*DEBUG*)
    fprintf out "(*Portée en VA dans tout le if : %s*)\n" (List.fold_left (fun a b -> a^" "^b) "" (list_of_scope(Hashtbl.find env "Scope")));
     
      let cur_scope_avant_then = list_of_scope (Hashtbl.find env  "Scope") in

    let loc_true = if is_none curloc  
      then (*Toplevel*) LIf(true, LDecl (-1)) (*-1 = représente un truc vide (informel)*)
      else (*Imbriqué*) loc_pushback (LIf(true, LDecl (-1)))  (Option.get curloc)
    in
    Hashtbl.replace env "Loc" (Loc (Some loc_true)); (*mise à jour de la loc actuelle*)
    prodcode out env vrai;     (* Traitement de la branche true *)

(*DEBUG*)
fprintf out "(*Portée en VA dans tout le if true : %s*)\n" (List.fold_left (fun a b -> a^" "^b) "" (list_of_scope(Hashtbl.find env "Scope")));
     

    (*Ajout dans le retour de tout ce qui est traité dans cette branche*)
    populate_liste_retour out env vrai;

    (*Nettoyage de la table*)
    filter_hashtable env loc_true;
    (*Rétablissement du scope avant le then*)
    Hashtbl.replace env  "Scope" (Scope cur_scope_avant_then);

    print out "\n end\n else begin\n"; (*false*)
    let loc_false = (if is_none curloc  
      then (*Toplevel*) LIf(false, LDecl (-1)) (*-1 = représente un truc vide (informel)*)
      else (*Imbriqué*) loc_pushback (LIf(false, LDecl (-1)))  (Option.get curloc)
    ) in
    Hashtbl.replace env "Loc" (Loc (Some loc_false)); (*mise à jour de la loc actuelle, autre choix*)
    prodcode out env faux; print out "\nend\n";
    
    (*DEBUG*)
fprintf out "(*Portée en VA dans tout le if false : %s*)\n" (List.fold_left (fun a b -> a^" "^b) "" (list_of_scope(Hashtbl.find env "Scope")));
     

    (*Ajout dans le retour de tout ce qui est traité dans cette branche*)
    populate_liste_retour out env faux;

    (*Nettoyage de la table*)
    filter_hashtable env loc_false; (*False ou true, pareil comme seul l'un des deux existe de tte façon*)

    (*Rétablissement du scope avant le else*)
    Hashtbl.replace env  "Scope" (Scope cur_scope);
    (*et de la loc*)
    Hashtbl.replace env "Loc" (Loc (curloc));
  
  and manage_metro_let out env x e =
    if not (is_dist env e) then failwith "Sample utilisé avec un objet qui n'est pas une distribution"; (*La suite immédiate doit être une distribution*)
    fprintf out "let %s = sample \"%s\" " x x;
    let curloc = locopt_of_loc (Hashtbl.find env "Loc") in
    let xloc = add_var_in_table env x curloc in (*Ajout de x dans les variables aléatoires*)
    fprintf out "(* LOC(%s) = %s *)" x (print_loc (vartype_VarInfo_loc (Hashtbl.find env x)));
    
      
    prodcode out env e;
    print out " in\n";
    let v = find_random_variable env x in 
    fprintf out " in\n let deps__%s = ref [] in 
    let membre_retour_sans_deps__%s = ((\"%s\", %s), %s) in\n" x x x (print_loc (vartype_VarInfo_loc v)) x;(*deps se remplit à l'exécution*)
 
    (*Par contre, x dépend des variables dans notre scope ? *)
    add_to_deps env out x xloc  (find_vars e); (*ajout de x dans les variables dépendant de var, car var est dans les arguments du sample*)
    (*Et les variables du scope*)
    
    add_to_deps env out x xloc ( list_of_scope (Hashtbl.find env  "Scope") );
     
    (*Le let ne se "referme" jamais de son initiative (voir le cas du if à ce sujet)*)

 

  (*and smetro_generate_resample_variant out env e = prodcode out env e*)
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
  let cmd = sprintf  "true ocamlfind ocamlc -o \"%s\" %s \"%s\"" exefile (match length deps with
                                                                    |0 -> ""
                                                                    |_ -> sprintf "-package %s -linkpkg" deps
                                                                    )                                         mlfile in
  printf "$ > %s" cmd;
  match Sys.command cmd with
  |0 -> printf "Sortie : %s (dépendant de %s)\n" exefile deps
  |n -> printf "Erreur %d\n" n;
;;
