%{
  open File
%}
%token PLUS MOINS FOIS DIV PUISS EQ NEQ LESSE GREATE LESST GREATT MOD REM AND OR XOR THEN ELSE ABS NOT LPAR RPAR /*Token terminaux sans type*/
%token <float> CST
%token <string> ID
%start s
%type <File.expr> s

%left AND OR XOR AND THEN OR ELSE
%left EQ NEQ LESSE LESST GREATE GREATT
%left PLUS MOINS
%right FOIS DIV MOD REM
%nonassoc PUISS NOT ABS



%%

s: e {$1}

e:
    |e PLUS e { Plus($1,$3) }
    |e FOIS e { Fois($1,$3) }
    |LPAR e MOINS e RPAR { Moins($2,$4) }
    |LPAR e DIV e RPAR { Div($2,$4) }
    |LPAR e RPAR {$2}
    |LPAR e PUISS e RPAR { Puiss($2,$4) }
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
    |LPAR NOT e RPAR { Not($3) }
    

