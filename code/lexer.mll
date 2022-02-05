{ 
    open Printf
    open Parser 
}

rule decoupe = parse 
  |"mod"|"MOD" {MOD}
  |"rem"|"REM" {REM}
  |"and"|"AND" {AND}
  |"or"|"OR" {OR}
  |"xor"|"XOR" {XOR}
  |"abs"|"ABS" {ABS}
  |"not"|"NOT" {NOT}
  |"then"|"THEN" {THEN}
  |"else"|"ESLE" {ELSE}
  |"loop"|"LOOP" {LOOP}
  |"end"|"END" {END}
  |"while"|"WHILE" {WHILE} 
  |"for"|"FOR" {FOR}
  |"reverse"|"REVERSE" {REVERSE}
  |"in"|"IN" {IN}
  |"if"|"IF" {IF}
  |"elsif"|"ELSIF" {ELSIF}
  |"else"|"ELSE" {ELSE}
  |"case"|"CASE" {CASE}
  |"when"|"WHEN" {WHEN}
  |"others"|"OTHERS" {OTHERS}
  |"goto"|"GOTO" {GOTO}
  |"exit"|"EXIT" {EXIT}
  |"return"|"RETURN" {RETURN}
  |"range"|"RANGE" {RANGE}

  |"constant"|"CONSTANT" {CONSTANT}
  |"type"|"TYPE" {TYPE}  
  |"is"|"IS" {IS}
  |"subtype"|"SUBTYPE" {SUBTYPE}
  |"renames"|"RENAMES" {RENAMES}  
  |"procedure"|"PROCEDURE" {PROCEDURE}
  |"out"|"OUT" {OUT}
  |"function"|"FUNCTION" {FUNCTION}  
  |"begin"|"BEGIN" {BEGIN}
  |"null"|"NULL" {NULL}
  |"!=" {NEQ}
  |"<=" {LESSE}
  |"<<" {DEB_ETIQ}
  |">>" {FIN_ETIQ}
  |"**" {PUISS}
  |">=" {GREATE}
  |":=" {AFFECT}  
  |"=>" {FLECHE}
  |".." {PP}
  |"--"[^'\r''\n']* {decoupe lexbuf}
  |'+' {PLUS}
  |'-' {MOINS}
  |'/' {DIV}
  |'*' {FOIS}
  |'=' {EQ}
  |'<' {LESST}
  |'>' {GREATT}
  |'(' {LPAR}
  |')' {RPAR}
  |',' {VIR}
  |';' {PVIR}
  |'.' {P}
  |':' {DP} 
  |'|' {SEP} 
  |[' ''\t''\r''\n']+ {decoupe lexbuf}
  |eof {EOF}
  |'-'?['0'-'9']+ as i {CST_INT (int_of_string i)}
  |['0'-'9']+('.'['0'-'9']+)?(['e''E']['0'-'9']+)? as f {CST_FLOAT (float_of_string f)}
  |'"'[^'"']*'"' as str {STR str}
  |['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9''.']* as s {ID s}
