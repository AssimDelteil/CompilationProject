type ast =
    |Plus of ast*ast
    |Moins of ast*ast
    |Fois of ast*ast
    |Div of ast*ast
    |Puiss of ast*ast
    |Eq of ast*ast
    |Neq of ast*ast
    |LessE of ast*ast
    |LessT of ast*ast
    |GreatE of ast*ast
    |GreatT of ast*ast
    |Mod of ast*ast
    |Rem of ast*ast
    |And of ast*ast
    |Or of ast*ast
    |Xor of ast*ast
    |AndThen of ast*ast
    |OrElse of ast*ast
    |Not of ast
    |Abs of ast 
    |Nega of ast
    |Int of int
    |Float of float
    |Id of string 

(*Type expression demandé*)
type expr = 
    |Plus of expr*expr
    |Moins of expr*expr
    |Fois of expr*expr
    |Div of expr*expr
    |Puiss of expr*expr
    |Eq of expr*expr
    |Neq of expr*expr
    |LessE of expr*expr
    |LessT of expr*expr
    |GreatE of expr*expr
    |GreatT of expr*expr
    |Mod of expr*expr
    |Rem of expr*expr
    |And of expr*expr
    |Or of expr*expr
    |Xor of expr*expr

    |Id of string 
    |Cst of ada_type 

    |Nega of expr
    |Abs of expr
    |Not of expr 

    |AndThen of expr*expr
    |OrElse of expr*expr

    |ConvOuAppelFct of string*(expr list) 
    |Paren of expr

type range = int list

(*Utilisé par For 
Représente "soit de deux expressions séparées par
.., soit d’un type" *)
type for_range =
    |Range of range
    |Expr of expr*expr

(* Utilisé par Case  
"Chaque choix est soit une expression, 
soit deux expressions séparées par .., 
soit le mot-clé others" *)
type case_choix = 
    |Expr of expr
    |Range of expr*expr
    |Other

type instr = 
    (*Met un (string option)* au début de chaque instr pour la potentielle étiquette : 
    "Une instr est une séquence potentiellement vide d’étiquettes constituées chacune
    d’un identifiant entre << et >>, suivie de :"
    A chaque fois que besoin de identifiant: met string *)
    |Null of (string option)
    |Affect of (string option)*string*expr
    |AppelProc of (string option)*string*(expr list) (*Procédure d'appel*)
    |Loop of (string option)*(string option)*(instr list)*(string option)
    |While of (string option)*(string option)*expr*(instr list)*(string option)
    (*3ème string option pour le reverse
    Utilise bPT_... pour représenter "soit de deux expressions séparées par
    .., soit d’un type" *)
    |For of (string option)*(string option)*string*(string option)*for_range*(instr list)*(string option)
    (*4ème terme pour les elif, 5ème pour le else*)
    |If of (string option)*expr*(instr list)* ((expr*(instr list)) list) *((instr list) option)
    (*3ème terme est liste d'alternative, composée de liste de choix et d'instrs*)
    |Case of (string option)*expr*( ((case_choix list)*instr list) list) 
    |Goto of (string option)*string
    |Exit of (string option)*(string option)*(expr option)
    (*Retour de procédure et de fonction*)
    |ReturnProc of (string option)
    |ReturnFct of (string option)*expr

type notnull_string_list = (*ce type sert à spécifier que la liste est non vide*)
    |Fin of string
    |List of string * notnull_string_list

(*On définit un type pour chaque élément optionnel de déclaration*)
type mode = 
    |Null
    |In
    |Out
    |In_out

type parametre_list =
    |LastPara of notnull_string_list * mode * string
    |ParaList of notnull_string_list * mode * string * parametre_list

type decla =
    |Objet of notnull_string_list * string option * (expr option) 
    |Type of string * expr * expr 
    |Sous_type of string * string * expr * expr
    |Rename of notnull_string_list * string * string
    |Procedure of string * parametre_list option
    |Function of string * parametre_list option * string
(*Procedure et Function désigne les spécifications comme on les trouverait dans une interface. Les définitions correspondantes sont les suivantes*)
    |DefProcedure of string * parametre_list option * (decla list) * instr list * (string option)
    |DefFunction of string * parametre_list option * string * (decla list) * instr list * (string option)

type file =
    |File of (decla list)*(instr list)
    

let print_etiquette eti = 
    watch eti with
    |None -> print_string ""
    |Some(str) -> print_string "<<" ^ str ^ ">>"

let print_sep l =
  List.iter print_string l

let rec print_sep_spec = function
  | [] -> ()
  | [x] -> print_string "|-"
  | x :: q -> print_string x; print_sep_spec q

let rec aff_expr_aux l e =
  print_sep_spec l;
  match e with
    |Plus(a1, a2) ->
        print_string "Plus\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |Moins(a1, a2) ->
        print_string "Moins\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |Fois(a1, a2) ->
        print_string "Fois\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |Div(a1, a2) ->
        print_string "Div\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |Puiss(a1, a2) ->
        print_string "Puiss\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |Eq(a1, a2) ->
        print_string "Eq\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |Neq(a1, a2) ->
        print_string "Neq\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |LessE(a1, a2) ->
        print_string "LessE\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |LessT(a1, a2) ->
        print_string "LessT\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |GreatE(a1, a2) ->
        print_string "GreatE\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |GreatT(a1, a2) ->
        print_string "GreatT\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |Mod(a1, a2) ->
        print_string "Mod\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |Rem(a1, a2) ->
        print_string "Rem\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |And(a1, a2) ->
        print_string "And\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |Or(a1, a2) ->
        print_string "Or\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |Xor(a1, a2) ->
        print_string "Xor\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |AndThen(a1, a2) ->
        print_string "AndThen\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |OrElse(a1, a2) ->
        print_string "OrElse\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a1;
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["  "]) a2
    |Not (a) ->        
        print_string "Not\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a;
    |Abs (a) ->        
        print_string "Abs\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a;
    |Nega (a) ->        
        print_string "Nega\n";
        print_sep (l @ ["|\n"]);
        aff_aux (l @ ["| "]) a;
    |Int i -> Printf.printf "Cte(%i)\n" i
    |Float f -> Printf.printf "Cte(%f)\n" f
    |Id s -> Printf.printf "Id(%s)\n" s

let aff_expr e = aff_expr_aux [] e

let rec aff_expr_list l e_list = 
    match e_list with
    |[]-> print_string ""
    |e::e_list' ->  
        aff_expr_aux l e;
        aff_expr_list l e_list';

(*Print les string option comme des Id*)
let print_option opt = 
    watch opt with
    |None -> print_string ""
    |Some(str) -> print_string "Id("^str^")" 

(*Affiche une range, est une liste d'entier continue, [2;...;10] est affiché 2..10*)
let print_range range = 
    let rec print_range_aux i_list=
        match i_list with 
        |[]-> assert false (*ne peut pas atteindre ce point car check si pas vide avant et s'arrete à liste à un élt*)
        |[i] -> i
        |i::l -> print_range_aux l
    in
    match range with
    |[]->print_string " .. "
    |[i] -> Format.printf "%i..%i" i i 
    |i::l-> Format.printf "%i..%i" i (print_range_aux l) 

(*Affiche la range d'un for, peut être un range, ou etre composé de deux expr*)
let print_for_range l range = 
    match range with
    |Range(r)->
        print_sep_spec l;
        print_range r
    |Expr(e1,e2)->
        aff_expr_aux l e1
        print_sep (l @ ["|\n"]);
        print_string ".."
        print_sep (l @ ["|\n"]);
        aff_expr_aux l e2

(*Définie les fct en même temp (avec and) car s'appellent l'une l'autre*)
let rec aff_instr_list l i_list = 
    match i_list with
    |[]-> print_string ""
    |i::i_list' -> 
        aff_instr l i;
        print_sep (l @ ["|\n"]);
        aff_instr_list l i_list';

and rec aff_elif_list l elif_list =
    match elif_list with
    |[]-> print_string ""
    |(e,i_list)::elif_list' -> 
        print_sep_spec l;
        print_string "Else if\n";
        print_sep (l @ ["|\n"]); 
        aff_expr_aux l e;
        print_sep (l @ ["|\n"]);
        aff_instr_list l i_list;
        aff_elif_list l elif_list'    

and aff_else l option_else = 
    match option_else with
    |None -> print_string ""
    |Some(i_list) -> aff_instr_list l i_list

and aff_case_list l c_list = 
    let print_case_choix_list cc_list=
    match cc_list with 
    |[]->print_string ""
    |choix::cc_list' ->
        match choix with
        |Expr(e) -> aff_expr_aux l e ;
        |Range(e1,e2) ->
            aff_expr_aux l e1;
            print_sep (l @ ["|\t..\n"]);  
            aff_expr_aux l e2;
        |Other -> print_sep_spec l;
            print_string "Other\n";
    in
    match c_list with
    |[]-> print_string ""
    |(case_choix_list,i_list)::c_list' -> 
        print_case_choix_list case_choix_list;
        print_sep (l @ ["|\t..\n"]); 
        aff_instr_list l i_list;

and aff_instr l i = 
    print_sep_spec l;
    match i with
    |Null(eti) -> 
        print_etiquette eti
        print_string "Null\n";
        print_sep (l @ ["|\n"]);
    |Affect(eti, id, expr) ->
        print_etiquette eti;
        print_string "Affect\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(id);
        print_sep (l @ ["|\n"]);
        aff_expr_aux l expr;
    |AppelProc(eti,id,e_list) ->
        print_etiquette eti;
        print_string "AppelProc\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(id);
        print_sep (l @ ["|\n"]);
        aff_expr_list l e_list;
    |Loop(eti, nom_boucle, id, i_list, nom_end) ->
        print_etiquette eti;
        print_option nom_boucle;
        print_string "Loop\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(id);
        print_sep (l @ ["|\n"]);
        aff_instr_list l i_list;
        print_sep (l @ ["|\n"]);
        print_option nom_end;
    |While(eti,nom_boucle, e, i_list, nom_end) ->
        print_etiquette eti;
        print_option nom_boucle;
        print_string "While\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l e;
        print_sep (l @ ["|\n"]);
        aff_instr_list l i_list;
        print_sep (l @ ["|\n"]);
        print_option nom_end;
    |For(eti, nom_boucle, id, rev_option, range_for, i_list, nom_end) ->
        print_etiquette eti;
        print_option nom_boucle;
        print_string "For\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(id);
        print_option rev_option;
        print_sep (l @ ["|\n"]);
        print_for_range l range_for;
        print_sep (l @ ["|\n"]);
        aff_instr_list l i_list;
        print_sep (l @ ["|\n"]);
        print_option nom_end;
    |If(eti,e,i_list,elif_list, option_else) ->
        print_etiquette eti;
        print_string "If\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l e;
        print_sep (l @ ["|\n"]);
        aff_instr_list l i_list;
        print_sep (l @ ["|\n"]);
        aff_elif_list l elif_list;
        aff_else l option_else;
    |Case(eti,e,case_list) ->
        print_etiquette eti;
        print_string "Case\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l e;
        print_sep (l @ ["|\n"]);
        aff_case_list l e;
    |Goto(eti,id) ->
        print_etiquette eti;
        print_string "Goto\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(id);
    |Exit(eti,id_option,e_option) ->
        print_etiquette eti;
        print_string "Exit\n";
        match id_option with 
        |None -> print_string ""
        |Some(id) -> 
            print_sep (l @ ["|\n"]);
            print_sep_spec l;
            print_option id_option; (*Possible erreur ici, met le match dans le Some(id)*)
        match e_option with 
        |None -> print_string ""
        |Some(e) -> 
            print_sep (l @ ["|\n"]);
            aff_expr_aux l e;
    |ReturnProc(eti) ->
        print_etiquette eti;
        print_string "ReturnProc\n";
    |ReturnFct(eti,e) ->
        print_etiquette eti;
        print_string "AppelProc\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l e; 

let rec print_notnull_string_list nn_str_list=
    match nn_str_list with
    |[] -> assert false (*list est non nulle et condition d'arret à un élt, on est jamais censé arriver ici*)
    |[str] -> print_string str^"|"
    |str::nn_str_list' ->  print_string str^"|";
        print_notnull_string_list nn_str_list'

let print_mode m =
    print_sep_spec l 
    match m with 
    |Null -> print_string "Null"
    |In -> print_string "In"
    |Out -> print_string "Out"
    |In_out -> print_string "In_out"

let rec print_param param_list = 
    match param_list with
    |LastPara(nn_str_list, m, id)->
        print_sep_spec l ;
        print_notnull_string_list nn_str_list;
        print_sep (l @ ["|\n"]);
        print_mode m;
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(id);
    |ParaList(nn_str_list, m, id, param_list') ->
        print_sep_spec l ;
        print_notnull_string_list nn_str_list;
        print_sep (l @ ["|\n"]);
        print_mode m;
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(id);

        print_param param_list'; 

let rec aff_decl l d=
    print_sep_spec l;
    match d with
    |Objet(nn_str_list, str_option, e_option)->
        print_option str_option
        print_string "Objet\n";
        print_sep (l @ ["|\n"]);
        print_sep_spec l;
        print_notnull_string_list nn_str_list;
        match e_option with 
        |None -> print_string ""
        |Some(e) -> 
            print_sep (l @ ["|\n"]);
            aff_expr_aux l e;
    |Type(str,e1,e2)->
        print_string "Type\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(str)
        print_sep (l @ ["|\n"]);
        aff_expr_aux l e1
        print_sep (l @ ["|\t..\n"]);
        aff_expr_aux l e2
    |Sous_type(str1,str2,e1,e2)->
        print_string "Sous_type\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(str1)
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(str2)
        print_sep (l @ ["|\n"]);
        aff_expr_aux l e1
        print_sep (l @ ["|\t..\n"]);
        aff_expr_aux l e2
    |Rename(nn_str_list,str1,str2)->
        print_string "Rename\n";
        print_sep (l @ ["|\n"]);
        print_sep_spec l;
        print_notnull_string_list nn_str_list;
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(str1)
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(str2)
    |Procedure(str, param_opt)->
        print_string "Procedure\n";
        print_sep (l @ ["|\n"]);
        aff_expr_aux l Id(str1)
        print_sep (l @ ["|\n"]);
        print_param l param;

let aff_file f =
    match f with
    |(d_list, i_list)->