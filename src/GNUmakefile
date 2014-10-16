
#####################################################################
# Name: GNUmakefile
# $Author$
# $Date$
# $Revision$
#
# Copyright (C) 2009-2010 Francois Carcenac, All Rights Reserved
#####################################################################

OCAML_LIB_DIR = /lib/ocaml/

OBJ_DIR = ../obj/
BIN_DIR=../bin/
SRC_DIR=.
C_LIB_DIR = 

#VPATH=./src/:./build

EXEC_FILE=mlisp
MAIN_SOURCE=toplevel.ml

OCAMLC=$(shell if [ `which ocamlc.opt` ] ; then echo ocamlc.opt ; else echo ocamlc; fi)
OCAMLOPT=$(shell if [ `which ocamlopt.opt` ] ; then echo ocamlopt.opt ; else echo ocamlopt; fi)
OCAMLDEP=ocamldep
OCAMLLEX=$(shell if [ `which ocamllex.opt` ] ; then echo ocamllex.opt ; else echo ocamllex; fi)
OCAMLYACC=ocamlyacc

INCLUDES=-I $(OBJ_DIR) -I $(OCAML_LIB_DIR) # all relevant -I options here
OCAMLFLAGS=$(INCLUDES) -g # add other options for ocamlc here
OCAMLOPTFLAGS= -w Ae $(INCLUDES) # add other options for ocamlopt here
OCAMLOPTLINK=$(OCAMLOPTFLAGS)


LEX_SRC=$(wildcard $(SRC_DIR)/*.mll)
LEXER_SRC=$(LEX_SRC:.mll=.ml)
LEXER_DUMP=$(LEXER_SRC:.ml=.dump)

LCI_LEXER_SRC= lexer.ml
LCI_PARSER= parser

#YACC_SRC=$(wildcard $(SRC_DIR)/*.mly) # pb with bisect, the definition
                                       # is replaced by the one below.
YACC_SRC=$(wildcard *.mly)
PARSER_SRC=$(YACC_SRC:.mly=.ml)

ML_FILES=$(wildcard $(SRC_DIR)/*.ml)
MLI_FILES=$(wildcard $(SRC_DIR)/*.mli)

C_FILES=$(wildcard $(SRC_DIR)/*.c)
H_FILES=$(wildcard $(SRC_DIR)/*.h)

CMI_FILES=$(subst $(SRC_DIR)/,,$(MLI_FILES:.mli=.cmi))
CMO_FILES=$(subst $(SRC_DIR)/,,$(ML_FILES:.ml=.cmo))
CMX_FILES=$(subst $(SRC_DIR)/,,$(ML_FILES:.ml=.cmx))
O_FILES=$(subst $(SRC_DIR)/,,$(C_FILES:.c=.o))
OBJS_FILES=$(CMI_FILES) $(CMO_FILES)
REQUIRED_FILES = $(O_FILES) $(CMX_FILES)
DUMP_FILES=$(MLI_FILES:.mli=.dump)
CMO_COMPILE=\
misc.cmo\
env.cmo\
term.cmo\
globals.cmo\
parser.cmo\
lexer.cmo\
subr.cmo\
toplevel.cmo

CMX_COMPILE=$(CMO_COMPILE:%.cmo=%.cmx)
ML_COMPILE=$(CMO_COMPILE:%.cmo=%.ml)

OCAML_NAT_LIB_DEPS =str.cmxa
OCAML_BYT_LIB_DEPS =$(OCAML_NAT_LIB_DEPS:.cmxa=.cma)

# profiling option
prof: PROFILE=-p 

prof: build

all: build

build: clean depend 
	@make native

# compilation to native-code
native: $(CMX_FILES)
	@echo ""
	@echo "**** Executable generation ****"
	$(OCAMLOPT) -o $(EXEC_FILE) -inline 50 $(PROFILE) $(OCAMLOPTLINK) $(OCAML_NAT_LIB_DEPS) $(CMX_COMPILE)
	@echo ""
	@echo "*********************************************************"
	@echo "**** THE APPLICATION HAS BEEN SUCCESSFULLY GENERATED ****"
	@echo "*********************************************************"
	@echo ""
	@test -d $(BIN_DIR) || mkdir $(BIN_DIR)
	@mv $(EXEC_FILE) $(BIN_DIR)

# Generation of lexer and parser
parser:
	@echo "**** Generation of the parser ****"
	@for f in $(YACC_SRC); do $(OCAMLYACC) -v $$f; done
	@for f in $(LEX_SRC); do $(OCAMLLEX) $$f; done
	@echo "**** Parsers Generation Complete ****"
	@echo ""

# Compiles the C/OCAML interface modules
interface: 
	@echo "**** Compilation of C files ****"
	@for f in $(C_FILES); do $(OCAMLOPT) -c $(OCAMLOPTFLAGS) $$f; done
	@for f in $(SRC_FILES)$(O_FILES); do mv $$f $(OBJ_DIR); done

# Generates the modules dependencies
depend: 
	make parser
	@echo "**** Generation of dependencies ****"
	@test -d $(OBJ_DIR) || mkdir $(OBJ_DIR)
	@cd $(SRC_DIR)/ && $(OCAMLDEP) *.ml *.mli > ./.mldepend
	@echo $(O_FILES) : $(subst $(SRC_DIR)/,,$(C_FILES)) > .cdepend
	@cat .cdepend .mldepend > .depend
	@echo "**** Dependencies Generation Complete ****"
	@echo ""

# Common Rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .c .o .dump

.ml.cmo:
	@echo $@ >> .cmo.compile
	$(OCAMLC) -c $(OCAMLFLAGS) $<
	@mv $(SRC_DIR)/$@ $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmo=.cmi) $(OBJ_DIR)

.mli.cmi:
	@echo $@ >> .cmi.compile
	$(OCAMLC) $(OCAMLFLAGS) -c $<

lexer.cmx: lexer.ml
	@echo $@ >> .cmx.compile
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) $< 
	@mv $(SRC_DIR)/$@ $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.cmi) $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.o) $(OBJ_DIR)

parser.cmx: parser.ml
	@echo $@ >> .cmx.compile
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) $< 
	@mv $(SRC_DIR)/$@ $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.cmi) $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.o) $(OBJ_DIR)

.ml.cmx:
	@echo $@ >> .cmx.compile
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) $< 
	@mv $(SRC_DIR)/$@ $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.cmi) $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.o) $(OBJ_DIR)

.c.o:
	@echo $@ >> .o.compile
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $<
	@mv $(SRC_DIR)/$@ $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.c=.o) $(OBJ_DIR)

.mli.dump:
	$(OCAMLDOC) $(OCAMLDOCFLAGS) $(shell cat .mli.dump) -dump $(SRC_DIR)/$(@F:.mli=.dump) $< $(SRC_DIR)/$(@F:.dump=.ml)
	@echo -load $(SDD_DIR)$(@F:.mli=.dump) > .mli.dump
	@mv $(SRC_DIR)/$(@F:.mli=.dump) $(SDD_DIR)

-include .depend

# Clean Up
clean:
	@echo "**** Cleaning Project ****"
	rm -f .cmx.compile
	rm -f .cmi.compile
	rm -f .cmo.compile
	rm -f .o.compile
	rm -f .mldepend
	rm -f .cdepend
	rm -f .depend
	rm -f *.output
	rm -f $(LCI_LEXER_SRC)
	rm -f $(LCI_PARSER).mli
	rm -f $(LCI_PARSER).ml
	rm -rf $(OBJ_DIR)
	rm -rf $(BIN_DIR)
	rm -rf $(SRC_DIR)/*.cm[ixo]
	@echo "**** Cleaning Complete ****"
	@echo ""

# Generates a  
build-debug: clean 
	make parser
	make depend 
	make byte

# Compilation of a bytecode application
byte: $(CMO_COMPILE)
	@echo "**** Bytecode generation ****"
	$(OCAMLC) -o $(EXEC_FILE) $(OCAMLFLAGS) $(OCAML_BYT_LIB_DEPS) $(CMO_COMPILE)
	@test -d $(BIN_DIR) || mkdir $(BIN_DIR)
	@mv $(EXEC_FILE) $(BIN_DIR)

