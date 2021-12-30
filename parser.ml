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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open File
# 34 "parser.ml"
let yytransl_const = [|
  257 (* PLUS *);
  258 (* MOINS *);
  259 (* FOIS *);
  260 (* DIV *);
  261 (* PUISS *);
  262 (* EQ *);
  263 (* NEQ *);
  264 (* LESSE *);
  265 (* GREATE *);
  266 (* LESST *);
  267 (* GREATT *);
  268 (* MOD *);
  269 (* REM *);
  270 (* AND *);
  271 (* OR *);
  272 (* XOR *);
  273 (* THEN *);
  274 (* ELSE *);
  275 (* ABS *);
  276 (* NOT *);
  277 (* LPAR *);
  278 (* RPAR *);
  279 (* EOL *);
    0|]

let yytransl_block = [|
  280 (* CST_FLOAT *);
  281 (* CST_INT *);
  282 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\005\000\005\000\003\000\005\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\004\000\002\000\002\000\004\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\025\000\024\000\026\000\
\027\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\000\000\000\000\000\000\000\000\000\000\000\004\000\005\000\
\007\000"

let yydgoto = "\002\000\
\009\000\010\000"

let yysindex = "\002\000\
\056\255\000\000\056\255\056\255\028\255\000\000\000\000\000\000\
\000\000\192\255\058\255\000\000\056\255\215\255\056\255\056\255\
\056\255\056\255\056\255\056\255\056\255\056\255\056\255\056\255\
\038\255\048\255\056\255\000\000\232\255\056\255\056\255\056\255\
\000\000\058\255\058\255\032\255\032\255\032\255\032\255\032\255\
\032\255\058\255\058\255\056\255\249\000\056\255\249\000\249\000\
\000\000\249\255\010\000\027\000\249\000\249\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\082\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\105\255\128\255\046\000\068\000\090\000\112\000\134\000\
\156\000\151\255\174\255\000\000\171\000\000\000\186\000\201\000\
\000\000\000\000\000\000\000\000\216\000\231\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\253\255"

let yytablesize = 518
let yytable = "\011\000\
\012\000\014\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\000\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\045\000\047\000\048\000\
\000\000\000\000\050\000\051\000\052\000\003\000\000\000\000\000\
\015\000\000\000\016\000\000\000\000\000\000\000\000\000\003\000\
\053\000\000\000\054\000\023\000\024\000\000\000\004\000\013\000\
\005\000\003\000\000\000\006\000\007\000\008\000\044\000\000\000\
\004\000\003\000\005\000\000\000\016\000\006\000\007\000\008\000\
\000\000\046\000\004\000\000\000\005\000\023\000\024\000\006\000\
\007\000\008\000\004\000\000\000\005\000\000\000\000\000\006\000\
\007\000\008\000\021\000\021\000\000\000\021\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\000\000\000\000\021\000\
\021\000\021\000\000\000\000\000\000\000\000\000\000\000\021\000\
\021\000\002\000\002\000\000\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\000\000\000\000\002\000\002\000\
\002\000\000\000\000\000\000\000\000\000\000\000\002\000\002\000\
\003\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\000\000\000\000\003\000\003\000\003\000\
\000\000\000\000\000\000\000\000\000\000\003\000\003\000\014\000\
\014\000\000\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\000\000\000\000\014\000\014\000\014\000\000\000\
\000\000\000\000\000\000\000\000\014\000\014\000\015\000\015\000\
\000\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\000\000\000\000\015\000\015\000\015\000\000\000\000\000\
\015\000\000\000\016\000\015\000\015\000\017\000\018\000\019\000\
\020\000\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\015\000\
\030\000\016\000\031\000\032\000\017\000\018\000\019\000\020\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\000\000\
\015\000\000\000\016\000\000\000\033\000\017\000\018\000\019\000\
\020\000\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\000\000\015\000\000\000\016\000\000\000\049\000\017\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\000\000\015\000\000\000\016\000\000\000\055\000\017\000\
\018\000\019\000\020\000\021\000\022\000\023\000\024\000\025\000\
\026\000\027\000\000\000\015\000\000\000\016\000\000\000\056\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\026\000\027\000\000\000\000\000\000\000\000\000\008\000\
\057\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\000\000\000\000\008\000\008\000\008\000\000\000\000\000\
\000\000\000\000\000\000\008\000\008\000\009\000\000\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\000\000\
\000\000\009\000\009\000\009\000\000\000\000\000\000\000\000\000\
\000\000\009\000\009\000\010\000\000\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\000\000\000\000\010\000\
\010\000\010\000\000\000\000\000\000\000\000\000\000\000\010\000\
\010\000\011\000\000\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\000\000\000\000\011\000\011\000\011\000\
\000\000\000\000\000\000\000\000\000\000\011\000\011\000\012\000\
\000\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\000\000\000\000\012\000\012\000\012\000\000\000\000\000\
\000\000\000\000\000\000\012\000\012\000\013\000\000\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\000\000\
\000\000\013\000\013\000\013\000\016\000\000\000\016\000\016\000\
\000\000\013\000\013\000\000\000\000\000\000\000\000\000\000\000\
\016\000\016\000\016\000\017\000\000\000\017\000\017\000\000\000\
\016\000\016\000\000\000\000\000\000\000\000\000\000\000\017\000\
\017\000\017\000\018\000\000\000\018\000\018\000\000\000\017\000\
\017\000\000\000\000\000\000\000\000\000\000\000\018\000\018\000\
\018\000\019\000\000\000\019\000\019\000\000\000\018\000\018\000\
\000\000\000\000\000\000\000\000\000\000\019\000\019\000\019\000\
\020\000\000\000\020\000\020\000\000\000\019\000\019\000\000\000\
\000\000\000\000\000\000\000\000\020\000\020\000\020\000\000\000\
\000\000\015\000\000\000\016\000\020\000\020\000\017\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000"

let yycheck = "\003\000\
\004\000\005\000\001\000\255\255\255\255\255\255\255\255\255\255\
\255\255\013\000\255\255\015\000\016\000\017\000\018\000\019\000\
\020\000\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\255\255\255\255\030\000\031\000\032\000\002\001\255\255\255\255\
\001\001\255\255\003\001\255\255\255\255\255\255\255\255\002\001\
\044\000\255\255\046\000\012\001\013\001\255\255\019\001\020\001\
\021\001\002\001\255\255\024\001\025\001\026\001\017\001\255\255\
\019\001\002\001\021\001\255\255\003\001\024\001\025\001\026\001\
\255\255\018\001\019\001\255\255\021\001\012\001\013\001\024\001\
\025\001\026\001\019\001\255\255\021\001\255\255\255\255\024\001\
\025\001\026\001\001\001\002\001\255\255\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\255\255\255\255\255\255\255\255\255\255\022\001\
\023\001\001\001\002\001\255\255\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\255\255\014\001\015\001\
\016\001\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\001\001\002\001\255\255\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\014\001\015\001\016\001\
\255\255\255\255\255\255\255\255\255\255\022\001\023\001\001\001\
\002\001\255\255\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\014\001\015\001\016\001\255\255\
\255\255\255\255\255\255\255\255\022\001\023\001\001\001\002\001\
\255\255\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\014\001\015\001\016\001\255\255\255\255\
\001\001\255\255\003\001\022\001\023\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\255\255\255\255\255\255\255\255\255\255\023\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\001\001\255\255\003\001\255\255\022\001\006\001\007\001\008\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\001\001\255\255\003\001\255\255\022\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\001\001\255\255\003\001\255\255\022\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\001\001\255\255\003\001\255\255\022\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\255\255\255\255\255\255\002\001\
\022\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\014\001\015\001\016\001\255\255\255\255\
\255\255\255\255\255\255\022\001\023\001\002\001\255\255\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\014\001\015\001\016\001\255\255\255\255\255\255\255\255\
\255\255\022\001\023\001\002\001\255\255\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\255\255\014\001\
\015\001\016\001\255\255\255\255\255\255\255\255\255\255\022\001\
\023\001\002\001\255\255\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\014\001\015\001\016\001\
\255\255\255\255\255\255\255\255\255\255\022\001\023\001\002\001\
\255\255\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\014\001\015\001\016\001\255\255\255\255\
\255\255\255\255\255\255\022\001\023\001\002\001\255\255\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\014\001\015\001\016\001\002\001\255\255\004\001\005\001\
\255\255\022\001\023\001\255\255\255\255\255\255\255\255\255\255\
\014\001\015\001\016\001\002\001\255\255\004\001\005\001\255\255\
\022\001\023\001\255\255\255\255\255\255\255\255\255\255\014\001\
\015\001\016\001\002\001\255\255\004\001\005\001\255\255\022\001\
\023\001\255\255\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\002\001\255\255\004\001\005\001\255\255\022\001\023\001\
\255\255\255\255\255\255\255\255\255\255\014\001\015\001\016\001\
\002\001\255\255\004\001\005\001\255\255\022\001\023\001\255\255\
\255\255\255\255\255\255\255\255\014\001\015\001\016\001\255\255\
\255\255\001\001\255\255\003\001\022\001\023\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001"

let yynames_const = "\
  PLUS\000\
  MOINS\000\
  FOIS\000\
  DIV\000\
  PUISS\000\
  EQ\000\
  NEQ\000\
  LESSE\000\
  GREATE\000\
  LESST\000\
  GREATT\000\
  MOD\000\
  REM\000\
  AND\000\
  OR\000\
  XOR\000\
  THEN\000\
  ELSE\000\
  ABS\000\
  NOT\000\
  LPAR\000\
  RPAR\000\
  EOL\000\
  "

let yynames_block = "\
  CST_FLOAT\000\
  CST_INT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 21 "parser.mly"
         (_1)
# 289 "parser.ml"
               : File.ast))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 24 "parser.mly"
              ( Plus(_1,_3) )
# 297 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 25 "parser.mly"
              ( Fois(_1,_3) )
# 305 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'e) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 26 "parser.mly"
                         ( Moins(_2,_4) )
# 313 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'e) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 27 "parser.mly"
                       ( Div(_2,_4) )
# 321 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 28 "parser.mly"
                 (_2)
# 328 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'e) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 29 "parser.mly"
                         ( Puiss(_2,_4) )
# 336 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 30 "parser.mly"
            ( Eq(_1,_3) )
# 344 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 31 "parser.mly"
             ( Neq(_1,_3) )
# 352 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 32 "parser.mly"
               ( LessE(_1,_3) )
# 360 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 33 "parser.mly"
                ( GreatE(_1,_3) )
# 368 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 34 "parser.mly"
               ( LessT(_1,_3) )
# 376 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 35 "parser.mly"
                ( GreatT(_1,_3) )
# 384 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 36 "parser.mly"
             ( Mod(_1,_3) )
# 392 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 37 "parser.mly"
             ( Rem(_1,_3) )
# 400 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 38 "parser.mly"
             ( And(_1,_3) )
# 408 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 39 "parser.mly"
            ( Or(_1,_3) )
# 416 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 40 "parser.mly"
             ( Xor(_1,_3) )
# 424 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'e) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 41 "parser.mly"
                  ( AndThen(_1,_4) )
# 432 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'e) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 42 "parser.mly"
                 ( OrElse(_1,_4) )
# 440 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 43 "parser.mly"
             ( Nega(_2) )
# 447 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 44 "parser.mly"
           ( Abs(_2) )
# 454 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 45 "parser.mly"
                     ( Not(_3) )
# 461 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 46 "parser.mly"
             ( Cst(_1) )
# 468 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 47 "parser.mly"
               ( Cst(_1) )
# 475 "parser.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "parser.mly"
        ( Id(_1) )
# 482 "parser.ml"
               : 'e))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : File.ast)
