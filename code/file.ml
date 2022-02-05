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
    

let print_etiquette eti = 
    match eti with
    |None -> print_string ""
    |Some(str) -> print_string ("<<" ^ str ^ ">>")

let print_sep l =
  List.iter print_string l

let rec aff_expr l e =
  print_sep (l@["|"]);
  let l = (l@["  "]) in 
  match e with
    |Plus(a1, a2) ->
        print_string "Plus\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |Moins(a1, a2) ->
        print_string "Moins\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |Fois(a1, a2) ->
        print_string "Fois\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |Div(a1, a2) ->
        print_string "Div\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |Puiss(a1, a2) ->
        print_string "Puiss\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |Eq(a1, a2) ->
        print_string "Eq\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |Neq(a1, a2) ->
        print_string "Neq\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |LessE(a1, a2) ->
        print_string "LessE\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |LessT(a1, a2) ->
        print_string "LessT\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |GreatE(a1, a2) ->
        print_string "GreatE\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |GreatT(a1, a2) ->
        print_string "GreatT\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |Mod(a1, a2) ->
        print_string "Mod\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |Rem(a1, a2) ->
        print_string "Rem\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |And(a1, a2) ->
        print_string "And\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |Or(a1, a2) ->
        print_string "Or\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |Xor(a1, a2) ->
        print_string "Xor\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |AndThen(a1, a2) ->
        print_string "AndThen\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |OrElse(a1, a2) ->
        print_string "OrElse\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a1;
        print_sep (l @ ["|\n"]);
        aff_expr l a2
    |Not (a) ->        
        print_string "Not\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a
    |Abs (a) ->        
        print_string "Abs\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a
    |Nega (a) ->        
        print_string "Nega\n";
        print_sep (l @ ["|\n"]);
        aff_expr l a
    |ConvOuAppelFct(id,e_list) ->
        print_string "ConvOuAppelFct\n";
        print_sep (l @ ["|\n"]);
        print_sep l;
        print_string ("|Id(" ^ id ^ ")\n");
        aff_expr_list l e_list
    |Int i -> Printf.printf "Cte(%i)\n" i
    |Float f -> Printf.printf "Cte(%f)\n" f
    |Id s -> Printf.printf "Id(%s)\n" s
    |Str str -> Printf.printf "Str(%s)\n" str

and aff_expr_list l e_list = 
    match e_list with
    |[]-> print_string ""
    |e::e_list' ->  
        print_sep (l @ ["|\n"]);
        aff_expr l e;
        aff_expr_list l e_list'

(*Print les string option comme des Id*)
let print_option opt = 
    match opt with
    |None -> print_string ""
    |Some(str) -> print_string ("Id("^str^")")

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

(*Définie les fct en même temp (avec and) car s'appellent l'une l'autre*)
let rec aff_instr_list l i_list = 
    match i_list with
    |[]-> print_string ""
    |i::i_list' -> 
        print_sep (l @ ["|\n"]);
        aff_instr l i;
        aff_instr_list l i_list'

and aff_elif_list l elif_list =
    match elif_list with
    |None-> 
        print_sep (l@["|\n"]);
        print_sep (l@["|"]);
        print_string "No elif\n"
    |Some(elif_list) ->
        match elif_list with 
        |[]->print_string ""
        |(e,i_list)::elif_list' -> 
            print_sep (l @ ["|\n"]); 
            print_sep l;
            print_string "|Else if\n";
            print_sep (l @ ["  |\n"]); 
            aff_expr (l@["  "]) e;
            aff_instr_list (l@["  "]) i_list;
            aff_elif_list l (Some(elif_list'))

and aff_else l option_else = 
    match option_else with
    |None -> 
        print_sep (l@["|\n"]);
        print_sep (l@["|"]);
        print_string "No else\n"
    |Some(i_list) -> 
        print_sep (l@["|\n"]);
        print_sep (l@["|"]);
        print_string "Else:\n";
        aff_instr_list (l@["  "]) i_list

and aff_case_list l c_list = 
    let print_case_choix_list cc_list=
        match cc_list with 
        |[]->print_string ""
        |choix::cc_list' ->
            match choix with
            |Expr(e) -> aff_expr l e 
            |Range(e1,e2) ->
                aff_expr l e1;
                print_sep (l @ ["|\t..\n"]);  
                aff_expr l e2
            |Other -> print_sep l;
                print_string "Other\n"
    in
    match c_list with
    |[]-> print_string ""
    |(case_choix_list,i_list)::c_list' -> 
        print_case_choix_list case_choix_list;
        print_sep (l @ ["|\t..\n"]); 
        aff_instr_list l i_list

and aff_instr l i = 
    print_sep (l@["|"]);
    match i with
    |NullInstr(eti) -> 
        print_etiquette eti;
        print_string "NullInstr\n";
    |Affect(eti, id, expr) ->
        print_etiquette eti;
        print_string "Affect\n";
        print_sep (l @ ["  |\n"]);
        aff_expr (l@["  "]) (Id(id));
        print_sep (l @ ["  |\n"]);
        aff_expr (l@["  "]) expr
    |AppelProc(eti,id,e_list_o) ->
        print_etiquette eti;
        print_string "AppelProc\n";
        print_sep (l @ ["  |\n"]);
        aff_expr (l@["  "]) (Id(id));
        (match e_list_o with
        |None -> print_string ""
        |Some(e_list) ->
            aff_expr_list (l@["  "]) e_list)
    |Loop(eti, nom_boucle, i_list, nom_end) ->
        print_etiquette eti;
        print_option nom_boucle;
        print_string "Loop\n";
        print_sep (l @ ["|\n"]);
        aff_instr_list l i_list;
        print_sep (l @ ["|\n"]);
        print_option nom_end
    |While(eti,nom_boucle, e, i_list, nom_end) ->
        print_etiquette eti;
        print_option nom_boucle;
        print_string "While\n";
        print_sep (l @ ["|\n"]);
        aff_expr l e;
        print_sep (l @ ["|\n"]);
        aff_instr_list l i_list;
        print_sep (l @ ["|\n"]);
        print_option nom_end
    |For(eti, nom_boucle, id, bool_rev_option, e1,e2, i_list, nom_end) ->
        print_etiquette eti;
        print_option nom_boucle;
        print_string "For\n";
        let l = (l@["  "]) in 
        print_sep (l @ ["|\n"]);
        aff_expr l (Id(id));
        print_sep (l @ ["|\n"]);

        print_sep l;
        print_string "|Range\n";
        print_sep (l @ ["  |\n"]);
        aff_expr (l@["  "]) e1;
        print_sep (l @ ["  |\n"]);
        aff_expr (l@["  "]) e2;

        print_sep (l @ ["|\n"]);
        print_sep l;
        Printf.printf "|Reverse : %b\n" bool_rev_option;

        print_sep (l @ ["|\n"]);
        print_sep l;
        print_string "|Do\n";
        aff_instr_list (l@["  "]) i_list;
        print_sep (l @ ["|\n"]);
        print_sep (l@["|"]);
        print_option nom_end;
        print_string "End For\n"
    |If(eti,e,i_list,elif_list, option_else) ->
        print_etiquette eti;
        print_string "If\n";
        let l = (l@["  "]) in 
        print_sep (l @ ["|\n"]);
        aff_expr l e;
        print_sep (l@["|\n"]);
        print_sep l;
        print_string "|Instructions:\n";
        aff_instr_list (l@["  "]) i_list;
        aff_elif_list l elif_list;
        aff_else l option_else;
        print_sep (l@["|\n"]);
        print_sep l;
        print_string "|End if\n"
    |Case(eti,e,case_list) ->
        print_etiquette eti;
        print_string "Case\n";
        print_sep (l @ ["|\n"]);
        aff_expr l e;
        print_sep (l @ ["|\n"]);
        aff_case_list l case_list
    |Goto(eti,id) ->
        print_etiquette eti;
        print_string "Goto\n";
        print_sep (l @ ["|\n"]);
        aff_expr l (Id(id))
    |Exit(eti,id_option,e_option) ->
        print_etiquette eti;
        print_string "Exit\n";
        (
        match (id_option, e_option) with 
        |(None, None) -> print_string ""
        |(None, Some(e))->
            print_sep (l @ ["|\n"]);
            aff_expr l e
        |(Some(id), None) -> 
            print_sep (l @ ["|\n"]);
            print_sep l;
            print_option id_option
        |(Some(id),Some(e)) ->
            print_sep (l @ ["|\n"]);
            print_sep l;
            print_option id_option;
            print_sep (l @ ["|\n"]);
            aff_expr l e
        )
    |ReturnProc(eti) ->
        print_etiquette eti;
        print_string "ReturnProc\n"
    |ReturnFct(eti,e) ->
        print_etiquette eti;
        print_string "ReturnFct\n";
        aff_expr (l@["  "]) e 

let rec print_notnull_string_list nn_str_list=
    match nn_str_list with
    |Fin(str) -> Printf.printf "%s\n" str
    |List(str,nn_str_list') ->  
        print_string (str^"|");
        print_notnull_string_list nn_str_list'

let print_mode m =
    match m with 
    |Null -> print_string "Null\n"
    |In -> print_string "In\n"
    |Out -> print_string "Out\n"
    |In_out -> print_string "In_out\n"

let print_param l param_list = 
    let rec print_p_list l p_list = 
        print_sep (l@["|\n"]);
        match p_list with
        |LastPara(nn_str_list, m, id)->
            print_sep (l@["|"]) ;
            print_notnull_string_list nn_str_list;
            print_sep (l@["|"]) ;
            print_mode m;
            aff_expr l (Id(id))
        |ParaList(nn_str_list, m, id, param_list') ->
            print_sep (l@["|"]) ;
            print_notnull_string_list nn_str_list;
            print_sep (l@["|"]) ;
            print_mode m;
            aff_expr l (Id(id));
            print_p_list l param_list'
    in 
    match param_list with
    |None -> 
        print_sep (l@["|"]);
        print_string "Pas de paramètres\n"
    |Some(p_list) -> 
        print_sep (l@["|"]);
        print_string "Paramètres:\n";
        print_p_list (l@["  "]) p_list

let aff_file f =

    let rec aff_delc_list l d_list = 
        match d_list with
        |[] -> print_string ""
        |d::d_list' -> 
            print_sep (l@["|\n"]);
            aff_decl l d;
            aff_delc_list l d_list'

    and aff_decl l d=
        print_sep (l@["|"]);
        match d with
        |Objet(id, const, id_opt, e_opt )->
            print_string "Objet\n";
            let l = (l@["  "]) in
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(id));
            print_sep (l @ ["|\n"]);
            print_sep l;
            Printf.printf "|Constant : %b\n" const;
            print_sep (l @ ["|\n"]);

            (match id_opt with 
            |None -> 
                print_sep l;
                print_string "|Pas de type\n"
            |Some(id) -> aff_expr l (Id(id));
            );

            print_sep (l @ ["|\n"]);
            (match e_opt with 
            |None -> 
                print_sep l;
                print_string "|Pas d'affectations\n"
            |Some(e) -> aff_expr l e)
        |Type(str,e1,e2)->
            print_string "Type\n";
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(str));
            print_sep (l @ ["|\n"]);
            aff_expr l e1;
            print_sep (l @ ["|\t..\n"]);
            aff_expr l e2
        |Sous_type(str1,str2,e1,e2)->
            print_string "Sous_type\n";
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(str1));
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(str2));
            print_sep (l @ ["|\n"]);
            aff_expr l e1;
            print_sep (l @ ["|\t..\n"]);
            aff_expr l e2
        |Rename(id,str1,str2)->
            print_string "Rename\n";
            let l = (l@["  "]) in
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(id));
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(str1));
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(str2))
        |Procedure(str, param_opt)->
            print_string "DéclaProcedure\n";
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(str));
            print_sep (l @ ["|\n"]);
            print_param l param_opt
        |Function(id, param_opt, id_type_retour) ->
            print_string "Function\n";
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(id));
            print_sep (l @ ["|\n"]);
            print_param l param_opt;
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(id_type_retour))
        |DefProcedure(id, param_opt, d_list_opt, i_list, str) ->
            print_string "DefProcedure\n";
            let l = (l@["  "]) in 
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(id));
            print_sep (l @ ["|\n"]);
            print_param l param_opt;
            print_sep (l @ ["|\n"]);
            (match d_list_opt with 
            |None -> 
                print_sep l;
                print_string "|Pas de déclarations\n"
            |Some(d_list)-> 
                print_sep l;
                print_string "|Déclarations:\n";
                aff_delc_list (l@["  "]) d_list);
            print_sep (l @ ["|\n"]);
            print_sep l;
            print_string "|Instructions:\n";
            aff_instr_list (l@["  "]) i_list
        |DefFunction(id, param_opt, id_type_retour, d_list_opt, i_list, str) ->
            print_string "DefFunction\n";
            let l = (l@["  "]) in 
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(id));
            print_sep (l @ ["|\n"]);
            print_param l param_opt;
            print_sep (l @ ["|\n"]);
            aff_expr l (Id(id_type_retour));
            print_sep (l @ ["|\n"]);
            (match d_list_opt with 
            |None -> 
                print_sep l;
                print_string "|Pas de déclarations\n"
            |Some(d_list)-> 
                print_sep l;
                print_string "|Déclarations:\n";
                aff_delc_list (l@["  "]) d_list );
            print_sep (l @ ["|\n"]);
            print_sep l;
            print_string "|Instructions :\n";
            aff_instr_list (l@["  "]) i_list;
            print_sep (l @ ["|\n"]);
            print_sep l;
            Printf.printf "|End %s\n" str

    in 
    match f with  
    |File(id,d_list_opt, i_list)-> 
        print_string "File\n";
        aff_expr [""] (Id(id));
        print_sep (["|\n"]);
        (match d_list_opt with 
        |None -> print_string "|Pas de déclarations\n"
        |Some(d_list)-> 
            print_string "|Déclarations:\n";
            aff_delc_list ["  "] d_list);
        print_sep (["|\n"]); 
        print_string "|Instructions:\n";
        aff_instr_list ["  "] i_list;
        print_string "End Of File\n"

(*

Unused functions about printing constants and checking scopes

let print_consts f = 
    let rec print_consts_expr e =
        match e with 
        |Plus(a1, a2)|Moins(a1, a2)|Fois(a1, a2)|Div(a1, a2)|Puiss(a1, a2)|Eq(a1, a2)
        |Neq(a1, a2)|LessE(a1, a2)|LessT(a1, a2)|GreatE(a1, a2)|GreatT(a1, a2)|Mod(a1, a2)
        |Rem(a1, a2)|And(a1, a2)|Or(a1, a2)|Xor(a1, a2)|AndThen(a1, a2)|OrElse(a1, a2)->
            print_consts_expr a1;
            print_consts_expr a2;
        |Not (a)|Abs (a)|Nega (a) ->        
            print_consts_expr a;
        |ConvOuAppelFct(id,e_list) ->
            print_consts_expr_list e_list;
        |Int i -> Printf.printf "%i\n" i;
        |Float f -> Printf.printf "%f\n" f;
        |Id s -> Printf.printf "";

    and print_consts_expr_list e_list = 
        match e_list with
        |[]-> Printf.printf "";
        |e::e_list' -> 
            print_consts_expr e;
            print_consts_expr_list e_list';
    in 
    let rec print_consts_instr_list i_list =
        match i_list with
        |[]-> Printf.printf "";
        |i::i_list' -> 
            print_consts_instr i;
            print_consts_instr_list i_list';
    
    and print_consts_instr i =
        let rec print_consts_elif_list elif_list = 
            match elif_list with 
            |None-> Printf.printf "" 
            |Some(elif_list) ->
                match elif_list with 
                |[]->print_string ""
                |(e,i_list)::elif_list' -> 
                    print_consts_expr e;
                    print_consts_instr_list i_list;
                    print_consts_elif_list (Some(elif_list'))
        in
        let rec print_consts_case_list case_list=
            let rec print_consts_case_choix_list case_choix_list = 
                match case_choix_list with
                |[] -> Printf.printf "" 
                |Expr(e)::ccl' -> 
                    print_consts_expr e;
                    print_consts_case_choix_list ccl'
                |Range(e1,e2)::ccl' -> 
                    print_consts_expr e1;
                    print_consts_expr e2;
                    print_consts_case_choix_list ccl';
                |Other::ccl' -> print_consts_case_choix_list ccl'
            in 
            match case_list with
            |[]->Printf.printf "" 
            |(case_choix_list,i_list)::case_list'->
                print_consts_case_choix_list case_choix_list;
                print_consts_instr_list i_list;
                print_consts_case_list case_list'
        in

        (*print_consts_instr strat here*) 
        match i with 
        |NullInstr(eti) -> Printf.printf "";
        |Affect(eti, id, e) -> print_consts_expr e
        |AppelProc(eti,id,e_list_o) -> 
            (match e_list_o with 
            |None -> print_string ""
            |Some(e_list) -> print_consts_expr_list e_list)
        |Loop(eti, nom_boucle, i_list, nom_end) -> print_consts_instr_list i_list;
        |While(eti,nom_boucle, e, i_list, nom_end) -> 
            print_consts_expr e;
            print_consts_instr_list i_list;
        |For(eti, nom_boucle, id, rev_option, range_for, i_list, nom_end) ->
            (match range_for with 
            |ForRange(str) -> Printf.printf "" 
            |ForExpr(e1,e2) -> 
                print_consts_expr e1;
                print_consts_expr e2);
            print_consts_instr_list i_list;
        |If(eti,e,i_list,elif_list, option_else) ->
            print_consts_expr e;
            print_consts_instr_list i_list;
            print_consts_elif_list elif_list;
            (match option_else with 
            |None->Printf.printf "" 
            |Some(else_i_list)-> print_consts_instr_list else_i_list);
        |Case(eti,e,case_list) ->
            print_consts_expr e;
            print_consts_case_list case_list;
        |Goto(eti,id) -> Printf.printf "" ;
        |Exit(eti,id_option,e_option) ->
            (match e_option with 
            |None -> print_string "";
            |Some(e)-> print_consts_expr e);
        |ReturnProc(eti) -> Printf.printf "";
        |ReturnFct(eti,e) -> print_consts_expr e;
    in 

    let rec print_consts_decla_list d_list =
        match d_list with
        |[]-> Printf.printf "";
        |d::d_list' -> 
            print_consts_decla d;
            print_consts_decla_list d_list';

    and print_consts_decla d =
        match d with
            |Objet(nn_str_list, str_option, e_option)->
                (match e_option with 
                |None -> print_string ""
                |Some(e) -> print_consts_expr e);
            |Type(str,e1,e2)->
                print_consts_expr e1;
                print_consts_expr e2;
            |Sous_type(str1,str2,e1,e2)->
                print_consts_expr e1;
                print_consts_expr e2;
            |Rename(nn_str_list,str1,str2)-> print_string "";
            |Procedure(str, param_opt)-> print_string "";
            |Function(id, param_opt, id_type_retour) -> print_string "";
            |DefProcedure(id, param_opt, d_list, i_list, str_opt) ->
                print_consts_decla_list d_list;
                print_consts_instr_list i_list;
            |DefFunction(id, param_opt, id_type_retour, d_list, i_list, str_opt) ->
                print_consts_decla_list d_list;
                print_consts_instr_list i_list;
    in 
    (*print_consts strat here*) 
    (match f with 
    |File(str,None,i_list)->
        Printf.printf "Constantes : \n";
        print_consts_instr_list i_list
    |File(str,Some(d_list),i_list)->
        Printf.printf "Constantes : \n";
        print_consts_decla_list d_list;
        print_consts_instr_list i_list)
    
let check_affect f = 
    (*get_forbiden_decla_list forbiden i_list -> check i_list with forbiden_list*)
    let rec get_forbiden_decla_list forbiden_list d_list =
        let rec add_nn_str_list nn_str_list forbiden_list = 
            match nn_str_list with 
            |Fin(str) -> str::forbiden_list
            |List(str,nnsl) -> add_nn_str_list nnsl (str::forbiden_list)
        in
        let rec update_forbiden list_forbiden param_opt =
            match param_opt with 
            |None->list_forbiden
            |Some(param_list)->     
                match param_list with 
                |LastPara(nnsl, m, _)-> 
                    (match m with 
                    |Out|In_out -> add_nn_str_list nnsl list_forbiden
                    |_ -> list_forbiden)
                |ParaList(nnsl,m,_,plist)->
                    (match m with 
                    |Out|In_out -> 
                        let lf = add_nn_str_list nnsl list_forbiden in
                        update_forbiden lf (Some(plist))
                    |_ -> update_forbiden list_forbiden (Some(plist))  
                    )
        in 
        match d_list with 
        |[]-> forbiden_list
        |d::d_list'->
            match d with 
            |Objet(nn_str_list, str_option, _)->
                (match str_option with 
                |None -> get_forbiden_decla_list forbiden_list d_list'
                |Some(_) -> 
                    let forbiden' = add_nn_str_list nn_str_list forbiden_list in
                    get_forbiden_decla_list forbiden' d_list')
            |DefProcedure(_, param_opt, d_list'', i_list, _) ->
                let lf = update_forbiden forbiden_list param_opt in 
                let lf' = get_forbiden_decla_list lf d_list'' in 
                if check_affect_instr_list lf' i_list
                then lf'
                else assert false (*if false, execption already raised *)
            |DefFunction(_, param_opt, _, d_list'', i_list, _) ->
                let lf = update_forbiden forbiden_list param_opt in 
                let lf' = get_forbiden_decla_list lf d_list'' in 
                if check_affect_instr_list lf' i_list
                then lf'
                else assert false (*if false, execption already raised *)
            |_-> forbiden_list
    
    
    and check_affect_instr_list forbiden_list i_list= 
        match i_list with 
        |[]->true
        |i::i_list' -> 
            (match i with 
            |Affect(_, id, _)-> 
                if List.mem id forbiden_list
                then raise (Affect_Not_Correct (forbiden_list,[i]))
                else true&&(check_affect_instr_list forbiden_list i_list')
            |_ -> check_affect_instr_list forbiden_list i_list')

    in
    match f with 
    |File(id, d_list_opt, i_list)->
        (match d_list_opt with
        |None -> check_affect_instr_list [] i_list
        |Some(d_list) -> 
            let forbiden = get_forbiden_decla_list [] d_list in
            check_affect_instr_list forbiden i_list)

let check_scope f = true

let print_conts_and_check f = true *)
