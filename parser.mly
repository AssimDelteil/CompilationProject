+%{
  open File
%}
%token PLUS MINUS STAR DVD EXP EQ NEQ LTEQ GTEQ LT GT MOD REM AND OR XOR THEN ELSE ABS NOT LPAR RPAR(*Token terminaux sans type*)
%token <int> CST
%token <float> CST
%token <string> ID
%start s
%type <File.expr> s


%left PLUS
%right STAR


%%

s: e {$1}

e:
    |e PLUS e { Plus($1,$3) }
    |e STAR e { Fois($1,$3) }
    |e MINUS e { Moins($1,$3) }
    |e DVD e { Div($1,$3) }
    |LPAR e RPAR {$2}
    |e EXP e { Puiss($1,$3) }
    |e EQ e { Eq($1,$3) }
    |e NEQ e { Neq($1,$3) }
    |e LTEQ e { LessE($1,$3) }
    |e GTEQ e { GreatE($1,$3) }
    |e LT e { LessT($1,$3) }
    |e GT e { GreatT($1,$3) }
    |e MOD e { Mod($1,$3) }   
    |e REM e { Rem($1,$3) }
    |e AND e { And($1,$3) }
    |e OR e { Or($1,$3) }
    |e XOR e { Xor($1,$3) }
    |e AND THEN e { AndThen($1,$4) }
    |e OR ELSE e { OrElse($1,$4) }
    |MINUS e { Nega($2) }
    |ABS e { Abs($2) }
    |NOT e { Not($2) }
    

