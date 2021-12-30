type ast =
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
    |Int of int
    |Float of float
    |String of string 

let print_sep l =
  List.iter print_string l

let rec print_sep_spec = function
  | [] -> ()
  | [x] -> print_string "|-"
  | x :: q -> print_string x; print_sep_spec q
    
let rec aff_aux l a =
  print_sep_spec l;
  match a with
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
    |Int i -> Printf.printf "Cte(%i)\n" i
    |Float f -> Printf.printf "Cte(%f)\n" f
    |String s -> Printf.printf "Id(%s)\n" s
    |Id s -> Printf.printf "Id(%s)\n" s

let affiche = aff_aux []

type file =
    |File of (decla list)*(instr list)

(*Cf discord pour les modifs a faire*)
type decla =
    |Objet of notnull_string_list * bool * (ada_type option) * (def option) (*bool est true si il y a constant, false sinon*)
    |Type of string * string * expr * expr * string (*Les deux premiers strings désignent "type" et l'identifiant*)
    |Sous_type of string * string * string * ada_type * string
    |Rename of notnull_string_list * ada_type * string
    |Procedure of string * parametre 
    |Function of string * parametre * ada_type

(*Procedure et Function désigne les spécifications comme on les trouverait dans une interface. Les définitions correspondantes sont les suivantes*)

    |DefProcedure of decla * string * (decla list) * string * instr list * string * (string option)
(*Le premier terme est une decla avec l'attribut Procedure*)
    |DefFunction of decla * string * (decla list) * string * instr list * string * (string option)
(*Le premier terme est une decla avec l'attribut Function*)

(*On définit un type pour chaque élément optionnel de déclaration*)
type mode = 
    |Null
    |In
    |Out
    |In_out

type parametre =
    |Null
    |Par of notnull_string_list * mode * ada_type * parametre

type notnull_string_list = (*ce type sert à spécifier que la liste est non vide*)
    |Fin of string
    |List of string * notnull_string_list

type ada_type =
    |Boolean of bool
    |Integer of int
    |Float of float
    |Character of char
    |Range of 'a list

type instr = 
    (*Met un (string option)* au début de chaque instr pour la potentielle étiquette : 
    "Une instr est une séquence potentiellement vide d’étiquettes constituées chacune
    d’un identifiant entre << et >>, suivie de :"
    A chaque fois que besoin de identifiant: met string *)
    |Null (string option)
    |Affect of (string option)*string*expr
    |Proc of (string option)*string*(expr list) (*Procédure d'appel*)
    (*B = Boucle*)
    |Loop of (string option)*(string option)*(instr list)*(string option)
    |While of (string option)*(string option)*expr*(instr list)*(string option)
    (*3ème string option pour le reverse
    Utilise bPT_... pour représenter "soit de deux expressions séparées par
    .., soit d’un type" *)
    |For of (string option)*(string option)*string*(string option)*bPT_type_ou_expr*(instr list)*(string option)
    (*4ème terme pour les elif, 5ème pour le else*)
    |If of (string option)*expr*(instr list)* ((expr*(instr list)) list) *((instr list) option)
    (*3ème terme est liste d'alternative, composée de liste de choix et d'instrs*)
    |Case of (string option)*expr*( ((case_choix list)*instr list) list) 
    |Goto of (string option)*string
    |Exit (string option)*(string option)*(expr option)
    (*Retour de procédure et de fonction*)
    |ReturnProc (string option)
    |ReturnFct of (string option)*expr

(*Utilisé par BPourTout 
Représente "soit de deux expressions séparées par
.., soit d’un type" *)
type bPT_type_ou_expr =
    |Type of ada_type
    |Expr of expr*expr

(* Utilisé par Case  
"Chaque choix est soit une expression, 
soit deux expressions séparées par .., 
soit le mot-clé others" *)
type case_choix = 
    |Expr of expr
    |Range of expr*expr
    |Other

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
    |Cst of cst 

    |Nega of expr
    |Abs of expr
    |Not of expr 

    |AndThen of expr*expr
    |OrElse of expr*expr

    |ConvOuAppelFct of string*(expr list) 
    |Paren of expr

type cst = 
    |Float of float
    |Int of int
    |String of string 
