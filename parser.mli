type token =
  | PLUS
  | MOINS
  | FOIS
  | DIV
  | PUISS
  | EQ
  | NEQ
  | LESSE
  | GREATE
  | LESST
  | GREATT
  | MOD
  | REM
  | AND
  | OR
  | XOR
  | THEN
  | ELSE
  | ABS
  | NOT
  | LPAR
  | RPAR
  | EOL
  | CST_FLOAT of (float)
  | CST_INT of (int)
  | ID of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> File.ast
