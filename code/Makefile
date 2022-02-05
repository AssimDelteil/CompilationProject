all: test_parser

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

lexer.ml: lexer.mll
	ocamllex $<

parser.ml: parser.mly
	ocamlyacc $<

parser.cmo: file.cmi parser.cmi

parser.cmi: parser.mli file.cmo
	ocamlc -c $<

test_parser: file.cmo parser.cmo lexer.cmo test_parser.cmo
	ocamlc -o $@ $^
	
clean:
	-rm lexer.ml parser.ml parser.mli *.cmo *.cmi test_parser

file.cmo: file.cmi
test_parser.cmo : parser.cmi lexer.cmo file.cmi