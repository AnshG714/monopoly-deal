modules=json_formation_util
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
JSONBUILD=json_formation_util.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default:
	build
	utop
	
build:
	$(OCAMLBUILD) $(OBJECTS)

edit_json:
	$(OCAMLBUILD) $(JSONBUILD) && ./$(JSONBUILD)