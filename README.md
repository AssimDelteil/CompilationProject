# asscom2021
Compile:
    lexer:
ocamllex lexer.mll
    parser:
ocamlyacc parser.mly
    file.ml(i)
ocamlc -c file.mli
ocamlc -c file.ml 