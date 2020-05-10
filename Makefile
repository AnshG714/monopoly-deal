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

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)	

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)


json:
	$(OCAMLBUILD) $(JSONBUILD) && ./$(JSONBUILD)

zip:
	zip deal_ms2.zip *.ml* INSTALL.txt *.json _tags Makefile 	

PKGS=unix,oUnit,yojson 