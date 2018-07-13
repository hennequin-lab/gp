byte:
	ocamlbuild -use-ocamlfind gp.cmo

native:
	ocamlbuild -use-ocamlfind gp.cmx
 
install:
	ocamlfind remove gp
	ocamlfind install gp META _build/gp.cmi _build/gp.cmo _build/gp.cmx _build/gp.o

uninstall:
	ocamlfind remove gp

clean:
	ocamlbuild -clean

all: clean byte native

