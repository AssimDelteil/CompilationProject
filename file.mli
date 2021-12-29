type file =
    |File of (declaration list)*(instruction list)

type mode = (*On définit un type pour chaque élément optionnel de déclaration*)
| Null
| in
| out
| in_out

type parametre =
| Null
| par of notnull_string_list * mode * ada_type * parametre

type notnull_string_list =
| Fin of string
| List of string * notnull_string_list

type ada_type =
| Boolean
| Integer
| Float
| Character

type declaration =
| decl_objet of notnull_string_list * bool * (ada_type option) * (def option)
| decl_type of string * string * expr * expr * string
| decl_sous_type of string * string * string * ada_type * string
| rename of notnull_string_list * ada_type * string * string 
| procedure of string * parametre
| ada_function of string * parametre * ada_type
| decl_procedure of procedure * string * declaration list * string * instruction list * string * (string option)
| decl_function of ada_function * string * declaration list * string * instruction list * string * (string option)

type instruction = 
    (*Met un (string option)* au début de chaque instruction pour la potentielle étiquette : 
    "Une instruction est une séquence potentiellement vide d’étiquettes constituées chacune
    d’un identifiant entre << et >>, suivie de :"
    A chaque fois que besoin de identifiant: met string *)
    |Null (string option)
    |Affect of (string option)*string*expr
    |Proc of (string option)*string*(expr list) (*Procédure d'appel*)
    (*B = Boucle*)
    |BSimple of (string option)*(string option)*(instruction list)*(string option)
    |BTantQue of (string option)*(string option)*expr*(instruction list)*(string option)
    (*3ème string option pour le reverse
    Utilise bPT_... pour représenter "soit de deux expressions séparées par
    .., soit d’un type" *)
    |BPourTout of (string option)*(string option)*string*(string option)*bPT_type_ou_expr*(instruction list)*(string option)
    (*4ème terme pour les elif, 5ème pour le else*)
    |If of (string option)*expr*(instruction list)* ((expr*(instruction list)) list) *((instruction list) option)
    (*3ème terme est liste d'alternative, composée de liste de choix et d'instructions*)
    |Case of (string option)*expr*( ((case_choix list)*instruction list) list) 
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
    |Cst of float (*Pas sur que ça soit float, implémentation compliquée, à revoir *)

    |Nega of expr
    |Abs of expr
    |Not of expr 

    |AndThen of expr*expr
    |OrElse of expr*expr

    |ConvOuAppelFct of string*(expr list) 
    |Paren of expr
 
