{ 
    open Printf
    open Parser 
}

rule decoupe = parse 
  |'+' {PLUS}
  |'-' {MOINS}
  |"**" {PUISS}
  |'/' {DIV}
  |'*' {FOIS}
  |'=' {EQ}
  |"!=" {NEQ}
  |"<=" {LESSE}
  |'<' {LESS}
  |">=" {GREATE}
  |'>' {GREATT}
  |"mod" {MOD}
  |"rem" {REM}
  |"and" {AND}
  |"or" {OR}
  |"xor" {XOR}
  |"abs" {ABS}
  |"not" {NOT}
  |"then" {THEN}
  |"else" {ELSE}
  |"<<" {DEB_ETIQ}
  |">>" {FIN_ETIQ}
  |"null" {NULL}
  |":=" {AFFECT}  
  |'(' {LPAR}
  |')' {RPAR}
  |',' {VIR}
  |';' {PVIR}
  |'.' {P}
  |':' {DP} 
  |"loop" {LOOP}
  |"end" {END}
  |"while" {WHILE} 
  |"for" {FOR}
  |"reverse" {REVERSE}
  |"in" {IN}
  |".." {PP}
  |"if" {IF}
  |"elsif" {ELSIF}
  |"else" {ELSE}
  |"case" {CASE}
  |"when" {WHEN}
  |'|' {SEP}
  |"=>" {FLECHE}
  |"others" {OTHERS}
  |"goto" {GOTO}
  |"exit" {EXIT}
  |"return" {RETURN}
  |"range" {RANGE}
  |"Integer" {INTEGER}  
  |"Boolean" {BOOLEAN}
  |"constant" {CONSTANT}
  |"type" {TYPE}  
  |"is" {IS}
  |"subtype" {SUBTYPE}
  |"renames" {RENAMES}  
  |"procedure" {PROCEDURE}
  |"out" {OUT}
  |"function" {FUNCTION}  
  |"begin" {BEGIN}
  |'\n' {EOL}
  |[' ''\t']+ {decoupe lexbuf}
  |"--" {COMM}
  |['0'-'9']+ as i {CST (int_of_string i)}
  |['0'-'9']+(.['0'-'9']*)?['e'|'E']['0'-'9']]?