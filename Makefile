BIN = main
JSDIR = js

all: types.ml lexer.mll parser.mly rules.ml check.ml main.ml
	ocamlc types.ml
	ocamllex lexer.mll
	menhir parser.mly
	ocamlc -o $(BIN) types.cmo parser.mli lexer.ml parser.ml rules.ml check.ml main.ml

js: all
	mkdir -p $(JSDIR)
	ocamlc types.ml
	ocamllex lexer.mll
	menhir parser.mly
	ocamlfind ocamlc -g -package js_of_ocaml -package js_of_ocaml-ppx -linkpkg types.cmo parser.mli lexer.ml parser.ml rules.ml check.ml js_main.ml -o jsmain.byte
	js_of_ocaml --pretty jsmain.byte
	mv jsmain.js $(JSDIR)
	echo -n 'var compiled = "' > $(JSDIR)/version.js
	date | tr -d '\n' >> $(JSDIR)/version.js
	echo '"' >> $(JSDIR)/version.js

clean:
	rm -f *~
	rm -f *.cmo
	rm -f *.cmi
	rm -f $(BIN)
	rm -f *.tmp
	rm -f lexer.ml
	rm -f parser.ml
	rm -f a.out
	rm -f jsmain.byte

clean-js: clean
	rm -rf $(JSDIR)
