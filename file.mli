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
    |Int of int 
    |Float of float 
    |Str of string

    |Nega of expr
    |Abs of expr
    |Not of expr 

    |AndThen of expr*expr
    |OrElse of expr*expr

    |ConvOuAppelFct of string*(expr list) 

type range = int list

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
    |NullInstr of (string option)
    |Affect of (string option)*string*expr 
    |AppelProc of (string option)*string*(expr list) option(*Procédure d'appel*)
    (*B = Boucle*)
    |Loop of (string option)*(string option)*(instr list)*(string option)
    |While of (string option)*(string option)*expr*(instr list)*(string option)
    (*bool pour le reverse (si true: alors reverse)
    Utilise bPT_... pour représenter "soit de deux expressions séparées par
    .., soit d’un type" *)
    |For of (string option)*(string option)*string*bool*expr*expr*(instr list)*(string option)
    (*4ème terme pour les elif, 5ème pour le else*)
    |If of (string option)*expr*(instr list)* (((expr*(instr list)) list) option) *((instr list) option)
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

type parametre_list =
    |LastPara of notnull_string_list * mode * string
    |ParaList of notnull_string_list * mode * string * parametre_list

type decla =
    |Objet of string * bool * (string option) * (expr option)
    |Type of string * expr * expr 
    |Sous_type of string * string * expr * expr
    |Rename of string * string * string
    |Procedure of string * parametre_list option
    |Function of string * parametre_list option * string
(*Procedure et Function désigne les spécifications comme on les trouverait dans une interface. Les définitions correspondantes sont les suivantes*)
    |DefProcedure of string * parametre_list option * (decla list) option * instr list * string 
    |DefFunction of string * parametre_list option * string * (decla list) option * instr list * string
(*Fichier est une procédure: contient nom de la procédure, des déclarations optionnelles et des instructions*)
type file =
    |File of string*(decla list) option*(instr list)

exception Affect_Not_Correct of string list * instr list

val aff_instr_list : string list -> instr list -> unit  
(*@requires nothing
@ensures print ast of file*)
val aff_file : file -> unit

(*

Unused functions about printing constants and checking scopes

(*@requires nothing
@ensures print constant of ast*)
val print_consts : file -> unit 

(*@requires nothing
@ensures return true if affectations are correct
@raises Not_correct(str) with str containing information on why the affect is not correct*)
val check_affect : file -> bool 

(*@requires nothing
@ensures return true if scopes are correct
@raises Not_correct(str) with str containing information on why there is a probleme with scope*)
val check_scope : file -> bool 

(*@requires nothing
@ensures return true if check_scope and check_affect returns true 
    And prints all constant in the file
@raises Not_correct(str) with str containing information on why there is a probleme with scope or affect*)
val print_conts_and_check : file -> bool
*)
