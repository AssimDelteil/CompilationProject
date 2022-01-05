%{
  open File
%}
/*Token terminaux sans type*/
%token MOD REM AND OR XOR ABS NOT THEN ELSE LOOP END WHILE FOR REVERSE IN IF ELSIF ELSE CASE WHEN OTHERS GOTO EXIT RETURN RANGE INTEGER BOOLEAN CONSTANT TYPE IS SUBTYPE RENAMES PROCEDURE OUT FUNCTION BEGIN NULL NEQ LESSE DEB_ETIQ FIN_ETIQ PUISS GREATE AFFECT FLECHE PP COMMENTAIRE PLUS MOINS DIV FOIS EQ LESST GREATT LPAR RPAR VIR PVIR P DP SEP EOF EOL
%token <float> CST_FLOAT
%token <int> CST_INT
%token <string> ID
%start s
%type <File.file> s


%left LOOP END WHILE FOR REVERSE IF ELSIF WHEN OTHERS GOTO EXIT RETURN RANGE INTEGER BOOLEAN CONSTANT TYPE IS SUBTYPE RENAMES PROCEDURE OUT FUNCTION NULL DEB_ETIQ FIN_ETIQ AFFECT PP COMMENTAIRE LPAR RPAR VIR P DP SEP EOL EOF
%left AND OR XOR AND THEN OR ELSE
%left EQ NEQ LESSE LESST GREATE GREATT
%left PLUS MOINS PVIR 
%right FOIS DIV MOD REM
%nonassoc PUISS NOT ABS FLECHE IN




%%

s:s_prime EOF{$1}

s_prime: 
    |PROCEDURE ID IS d_list BEGIN i_list END ID PVIR {File($2,Some($4),$6)}
    |PROCEDURE ID IS BEGIN i_list END ID PVIR {File($2,None,$5)}


i_list:
  |i {[$1]}
  |i i_list{$1::$2}

d_list:
  |d {[$1]}
  |d d_list{$1::$2}

e:
    |e PLUS e { Plus($1,$2) }
    |e FOIS e { Fois($1,$2) }
    |e MOINS e { Moins($1,$2) }
    |e DIV e { Div($1,$2) }
    |LPAR e RPAR { $2 }
    |e PUISS e { Puiss($1,$2) }
    |e EQ e { Eq($1,$2) }
    |e NEQ e { Neq($1,$2) }
    |e LESSE e { LessE($1,$2) }
    |e GREATE e { GreatE($1,$2) }
    |e LESST e { LessT($1,$2) }
    |e GREATT e { GreatT($1,$2) }
    |e MOD e { Mod($1,$2) }   
    |e REM e { Rem($1,$2) }
    |e AND e { And($1,$2) }
    |e OR e { Or($1,$2) }
    |e XOR e { Xor($1,$2) }
    |e AND THEN e { AndThen($1,$4) }
    |e OR ELSE e { OrElse($1,$4) }
    |MOINS e { Nega($2) }
    |ABS e { Abs($2) }
    |NOT e { Not($2) }
    |CST_INT { Int($1) }
    |CST_FLOAT { Float($1) }
    |ID { Id($1) }
    |ID LPAR e_list RPAR { ConvOuAppelFct($1,$2) }

e_list:
  |e {[$1]}
  |e VIR e_list{$1::$2}


choix_for:
    |ID { ForRange($1) }
    |e PP e { ForExpr($1,$2) }



elsif_list:
    |ELSIF e THEN i_list { [($2,$4)] }
    |ELSIF e THEN i_list elsif_list { ($2,$4)::$5 }

case_choix:
    |e { Expr($1) }
    |e PP e { Range($1,$2) }
    |OTHERS { Other }

case_choix_list:
    |case_choix {[$1]}
    |case_choix SEP case_choix_list {$1::$2}

case_ligne:
    |case_choix_list FLECHE i_list PVIR {$1,$2}

case_ligne_list:
    |case_ligne {[$1]}
    |case_ligne case_ligne_list {$1::$2}

etiquette:
    | {None}
    |DEB_ETIQ ID FIN_ETIQ { Some($1) }

i:
    |etiquette NULL PVIR { NullInstr($1) }
    |etiquette NULL PVIR { NullInstr(None) }
    |etiquette ID AFFECT e PVIR { Affect($1,$2,$5) }
    |etiquette ID e_list PVIR { AppelProc($1,$2,$4) }
    |etiquette ID LOOP i_list END LOOP ID PVIR { Loop($1,Some($2),$5,Some($8)) }
    |etiquette ID WHILE e LOOP i_list END LOOP ID PVIR { While($1,Some($2),$5,$7,Some($10)) }
    |etiquette ID FOR ID IN REVERSE choix_for LOOP i_list END LOOP ID PVIR { For($1,Some($2),$5,true,$8,$10,Some($12)) }
    |etiquette ID FOR ID IN choix_for LOOP i_list END LOOP ID PVIR { For($1,Some($2),$5,false,$7,$9,Some($12)) }

    |etiquette IF e THEN i_list elsif_list END IF PVIR { If($1,$4,$6,Some($7),None) }
    |etiquette IF e THEN i_list ELSE i_list END IF PVIR { If($1,$4,$6,None,Some($8)) }
    |etiquette IF e THEN i_list elsif_list ELSE i_list END IF PVIR { If($1,$4,$6,Some($7),Some($9)) }
    |etiquette IF e THEN i_list END IF PVIR { If($1,$4,$6,None,None) }


    |etiquette CASE e IS case_ligne_list END CASE PVIR { Case($1,$4,$6) }
    |etiquette GOTO ID PVIR { Goto($1,$4) }
    |etiquette EXIT ID WHEN e PVIR { Exit($1,Some($4),Some($6)) }
    |etiquette EXIT PVIR { Exit($1,None,None) }
    |etiquette RETURN PVIR { ReturnProc($1) }
    |etiquette RETURN e PVIR {ReturnFct($1,$4) }


id_list:
  |ID { Fin($1) }
  |ID VIR id_list { List($1,$2) }

obj_choix:
  |DP CONSTANT ID { (Some($2)) }
  |DP CONSTANT {None}
  |DP ID { $1 }
  |DP {None}

mode:
    | { Null }
    |IN { In }
    |OUT { Out }
    |IN OUT { In_out }

parametre:
  |id_list DP mode ID { LastPara($1,$2,$4) }
  |id_list DP mode ID PVIR parametre { ParaList($1,$2,$4,$6) }

end_function:
  |END {None}
  |END ID { $1 }


d:
    |id_list obj_choix AFFECT e PVIR { Objet($1,$2,Some($4)) }
    |id_list obj_choix PVIR { Objet($1,$2,None) }
    |TYPE ID IS RANGE e PP e PVIR { Type($2,$5,$7) }
    |SUBTYPE ID IS ID RANGE e PP e PVIR { Sous_type($2,$4,$6,$8) }
    |id_list DP ID RENAMES ID PVIR { Rename($1,$2,$5) }

    |PROCEDURE ID LPAR parametre RPAR PVIR { Procedure($2,Some($4)) }
    |PROCEDURE ID PVIR { Procedure($2,None) }

    |FUNCTION ID LPAR parametre RPAR RETURN ID PVIR { Function($2,Some($4),$7) }
    |FUNCTION ID RETURN ID PVIR { Function($2,None,$4) }

    |PROCEDURE ID LPAR parametre RPAR IS d_list BEGIN i_list end_function PVIR { DefProcedure($2,Some($4),$7,$9,$10) }
    |PROCEDURE ID IS d_list BEGIN i_list end_function PVIR { DefProcedure($2,None,$4,$6,$7) }

    |FUNCTION ID LPAR parametre RPAR RETURN ID IS d_list BEGIN i_list end_function PVIR { DefFunction($2,Some($4),$7,$9,$11,$12) }
    |FUNCTION ID RETURN ID IS d_list BEGIN i_list end_function PVIR { DefFunction($2,None,$4,$6,$8,$9) }








    
    
