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

val affiche : ast -> unit



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
<<<<<<< HEAD
type for_range =
    |Range of range
=======
type bPT_type_ou_expr =
    |Range of string
>>>>>>> 68c86a906ae9bfe2bcd84178584948af0e3fb191
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
    (*B = Boucle*)
    |Loop of (string option)*(string option)*(instr list)*(string option)
    |While of (string option)*(string option)*expr*(instr list)*(string option)
    (*3ème string option pour le reverse
    Utilise bPT_... pour représenter "soit de deux expressions séparées par
    .., soit d’un type" *)
    |For of (string option)*(string option)*string*(string option)*for_range*(instr list)*(string option)
    (*4ème terme pour les elif, 5ème pour le else*)
    |If of (string option)*expr*(instr list)* ((expr*(instr list)) list) *((instr list) option)
    (*3ème terme est liste d'alternative, composée de liste de choix et d'instrs*)
    |Case of (string option)*expr*( ((case_choix list)*(instr list)) list) 
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

type parametre =
    |Fin of notnull_string_list * mode * string
    |Par of notnull_string_list * mode * string * parametre

type decla =
    |Objet of notnull_string_list * string option * (expr option) 
    |Type of string * expr * expr 
    |Sous_type of string * string * expr * expr
    |Rename of notnull_string_list * string * string
    |Procedure of string * parametre option
    |Function of string * parametre option * string
(*Procedure et Function désigne les spécifications comme on les trouverait dans une interface. Les définitions correspondantes sont les suivantes*)
    |DefProcedure of string * parametre option * (decla list) * instr list * (string option)
    |DefFunction of string * parametre option * string * (decla list) * instr list * (string option)



type file =
    |File of (decla list)*(instr list)

val affiche_file : file -> unit
