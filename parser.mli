type token =
  | MOD
  | REM
  | AND
  | OR
  | XOR
  | ABS
  | NOT
  | THEN
  | ELSE
  | LOOP
  | END
  | WHILE
  | FOR
  | REVERSE
  | IN
  | IF
  | ELSIF
  | CASE
  | WHEN
  | OTHERS
  | GOTO
  | EXIT
  | RETURN
  | RANGE
  | INTEGER
  | BOOLEAN
  | CONSTANT
  | TYPE
  | IS
  | SUBTYPE
  | RENAMES
  | PROCEDURE
  | OUT
  | FUNCTION
  | BEGIN
  | NULL
  | NEQ
  | LESSE
  | DEB_ETIQ
  | FIN_ETIQ
  | PUISS
  | GREATE
  | AFFECT
  | FLECHE
  | PP
  | COMM
  | PLUS
  | MOINS
  | DIV
  | FOIS
  | EQ
  | LESST
  | GREATT
  | LPAR
  | RPAR
  | VIR
  | PVIR
  | P
  | DP
  | SEP
  | EOL
  | CST_INT of (int)
  | CST_FLOAT of (float)
  | ID of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> File.ast
