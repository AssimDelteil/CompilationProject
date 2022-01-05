%{
  open File
%}
/*Token terminaux sans type*/
%token MOD REM AND OR XOR ABS NOT THEN ELSE LOOP END WHILE FOR REVERSE IN IF ELSIF ELSE CASE WHEN OTHERS GOTO EXIT RETURN RANGE INTEGER BOOLEAN CONSTANT TYPE IS SUBTYPE RENAMES PROCEDURE OUT FUNCTION BEGIN NULL NEQ LESSE DEB_ETIQ FIN_ETIQ PUISS GREATE AFFECT FLECHE PP COMM PLUS MOINS DIV FOIS EQ LESST GREATT LPAR RPAR VIR PVIR P DP SEP EOL
%token <float> CST_FLOAT
%token <int> CST_INT
%token <string> ID
%start s
%type <File.file> s


%left LOOP END WHILE FOR REVERSE IF ELSIF WHEN OTHERS GOTO EXIT RETURN RANGE INTEGER BOOLEAN CONSTANT TYPE IS SUBTYPE RENAMES PROCEDURE OUT FUNCTION NULL DEB_ETIQ FIN_ETIQ AFFECT PP COMM LPAR RPAR VIR P DP SEP EOL
%left AND OR XOR AND THEN OR ELSE
%left EQ NEQ LESSE LESST GREATE GREATT
%left PLUS MOINS PVIR 
%right FOIS DIV MOD REM
%nonassoc PUISS NOT ABS FLECHE IN




%%

s: PROCEDURE ID IS d_list BEGIN i_list EOL {File($2,$4,$6)}

i_list:
  |i {[$1]}
  |i i_list{$1::$2}

d_list:
  |d {[$1]}
  |d d_list{$1::$2}

e:
    |e PLUS e { Plus($1,$3) }
    |e FOIS e { Fois($1,$3) }
    |e MOINS e { Moins($1,$3) }
    |e DIV e { Div($1,$3) }
    |LPAR e RPAR { $2 }
    |e PUISS e { Puiss($1,$3) }
    |e EQ e { Eq($1,$3) }
    |e NEQ e { Neq($1,$3) }
    |e LESSE e { LessE($1,$3) }
    |e GREATE e { GreatE($1,$3) }
    |e LESST e { LessT($1,$3) }
    |e GREATT e { GreatT($1,$3) }
    |e MOD e { Mod($1,$3) }   
    |e REM e { Rem($1,$3) }
    |e AND e { And($1,$3) }
    |e OR e { Or($1,$3) }
    |e XOR e { Xor($1,$3) }
    |e AND THEN e { AndThen($1,$4) }
    |e OR ELSE e { OrElse($1,$4) }
    |MOINS e { Nega($2) }
    |ABS e { Abs($2) }
    |NOT e { Not($2) }
    |CST_INT { Int($1) }
    |CST_FLOAT { Float($1) }
    |ID { Id($1) }
    |ID LPAR e_list RPAR { ConvOuAppelFct($1,$3) }

e_list:
  |e {[$1]}
  |e VIR e_list{$1::$3}


choix_for:
    |ID { ForRange($1) }
    |e PP e { ForExpr($1,$3) }

else_elsif:
    |ELSE i_list {$2} 
    |ELSIF e THEN i_list else_elsif { [If("",$2,$4,$5,)] }

case_choix:
    |e { Expr($1) }
    |e PP e { Range($1,$3) }
    |OTHERS {}

case_choix_list:
    |case_choix {[$1]}
    |case_choix SEP case_choix_list {$1::$3}

case_ligne:
    |case_choix_list FLECHE i_list PVIR {$1,$3}

case_ligne_list:
    |case_ligne {[$1]}
    |case_ligne case_ligne_list {$1::$2}


i:
    |DEB_ETIQ ID FIN_ETIQ NULL PVIR { Null($2) }
    |DEB_ETIQ ID FIN_ETIQ ID AFFECT e PVIR { Affect($2,$4,$6) }
    |DEB_ETIQ ID FIN_ETIQ ID e_list PVIR { AppelsProc($2,$4,$5) }
    |DEB_ETIQ ID FIN_ETIQ ID LOOP i_list END LOOP ID PVIR { Loop($2,$4,$6,$9) }
    |DEB_ETIQ ID FIN_ETIQ ID WHILE e LOOP i_list END LOOP ID PVIR { While($2,$4,$6,$8,$11) }
    |DEB_ETIQ ID FIN_ETIQ ID FOR ID IN REVERSE choix_for LOOP i_list END LOOP ID PVIR { For($2,$4,$6,$9,$11,$14) }
    |DEB_ETIQ ID FIN_ETIQ ID FOR ID IN choix_for LOOP i_list END LOOP ID PVIR { For($2,$4,$6,$8,$10,$13) }
    |DEB_ETIQ ID FIN_ETIQ IF e THEN i_list else_elsif END IF PVIR { If($2,$5,$7,$8) }
    |DEB_ETIQ ID FIN_ETIQ CASE e IS case_ligne_list END CASE PVIR { Case($2,$5,$7) }
    |DEB_ETIQ ID FIN_ETIQ GOTO ID PVIR { Goto($2,$5) }
    |DEB_ETIQ ID FIN_ETIQ EXIT ID WHEN e PVIR { Exit($2,$5,$7) }
    |DEB_ETIQ ID FIN_ETIQ EXIT PVIR { Exit($2) }
    |DEB_ETIQ ID FIN_ETIQ RETURN PVIR { ReturnProc($2) }
    |DEB_ETIQ ID FIN_ETIQ RETURN e PVIR {ReturnFct($2,$5) }

id_list:
  |ID { Fin($1) }
  |ID VIR id_list { List($1,$3) }

obj_choix:
  |DP CONSTANT ID { ($3) }
  |DP CONSTANT {}
  |DP ID { ($2) }
  |DP {}

mode:
    | { Null() }
    |IN { In() }
    |OUT { Out() }
    |IN OUT { In_Out() }

parametre:
  |id_list DP mode ID { Fin($1,$3,$4) }
  |id_list DP mode ID PVIR parametre { Par($1,$3,$4,$6) }

end_function:
  |END {}
  |END ID { ($2) }


d:
    |id_list obj_choix AFFECT e PVIR { Objet($1,$2,$4) }
    |id_list obj_choix PVIR { Objet($1,$2) }
    |TYPE ID IS RANGE e PP e PVIR { Type($2,$5,$7) }
    |SUBTYPE ID IS ID RANGE e PP e PVIR { Sous_type($2,$4,$6,$8) }
    |id_list DP ID RENAMES ID PVIR { Rename($1,$3,$5) }

    |PROCEDURE ID LPAR parametre RPAR PVIR { Procedure($2,$4) }
    |PROCEDURE ID PVIR { Procedure($2) }

    |FUNCTION ID LPAR parametre RPAR RETURN ID PVIR { Function($2,$4,$7) }
    |FUNCTION ID RETURN ID PVIR { Function($2,$4) }

    |PROCEDURE ID LPAR parametre RPAR IS d_list BEGIN i_list end_function PVIR { DefProcedure($2,$4,$7,$9,$10) }
    |PROCEDURE ID IS d_list BEGIN i_list end_function PVIR { DefProcedure($2,$4,$6,$7) }

    |FUNCTION ID LPAR parametre RPAR RETURN ID IS d_list BEGIN i_list end_function PVIR { Function($2,$4,$7,$9,$11,$12) }
    |FUNCTION ID RETURN ID IS d_list BEGIN i_list end_function PVIR { Function($2,$4,$6,$8,$9) }







    
    
