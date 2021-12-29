type file =

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
| function of string * parametre * ada_type
| decl_procedure of procedure * string * declaration list * string * instruction list * string * (string option)
| decl_function of function * string * declaration list * string * instruction list * string * (string option)

type instruction = 

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
    |Cst of float (*Pas sur que ça soit float, implémentation compliquée *)

    |Nega of expr
    |Abs of expr
    |Not of expr 

    |AndThen of expr*expr
    |OrElse of expr*expr

    |ConvOuAppelFct of string*(expr list) 
    |Paren of expr
 