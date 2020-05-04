MODULES=json_formation_util card deck player command board game
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=game.byte
JSONBUILD=json_formation_util.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default:
	build
	utop
	
build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

json:
	$(OCAMLBUILD) $(JSONBUILD) && ./$(JSONBUILD)

zip:
	zip deal_ms1.zip *.ml* INSTALL.txt *.json _tags Makefile 	

PKGS=unix,oUnit,yojson 