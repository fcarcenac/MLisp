
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

OCAMLC=$(shell if [ `which ocamlc.opt` ] ; then echo ocamlc.opt ; else echo ocamlc; fi)
OCAMLOPT=$(shell if [ `which ocamlopt.opt` ] ; then echo ocamlopt.opt ; else echo ocamlopt; fi)
OCAMLDEP=ocamldep
OCAMLLEX=$(shell if [ `which ocamllex.opt` ] ; then echo ocamllex.opt ; else echo ocamllex; fi)
OCAMLYACC=ocamlyacc

INCLUDES=-I $(OBJ_DIR) -I $(OCAML_LIB_DIR) # all relevant -I options here
OCAMLFLAGS=$(INCLUDES) # add other options for ocamlc here
OCAMLOPTFLAGS= -S -w Ae $(INCLUDES) -unsafe -noassert -inline 20 -fno-PIC -nodynlink # add other options for ocamlopt here
OCAMLOPTLINK=$(OCAMLOPTFLAGS)

SOURCES =\
globals.ml\
misc.ml\
stack.ml\
env.ml\
term.ml\
parser.mly\
lexer.mll\
subr.ml\
toplevel.ml

SOURCES1=$(SOURCES:.mly=.ml)
SOURCES2=$(SOURCES1:.mll=.ml)
CMX_COMPILE=$(SOURCES2:.ml=.cmx)
CMO_COMPILE=$(SOURCES2:.ml=.cmo)

ML_COMPILE=$(CMO_COMPILE:%.cmo=%.ml)

OCAML_NAT_LIB_DEPS =
OCAML_BYT_LIB_DEPS =$(OCAML_NAT_LIB_DEPS:.cmxa=.cma)

# profiling option
#prof: PROFILE=-p 

#prof: $(EXEC_FILE)

all: depend $(EXEC_FILE)

opt: $(EXEC_FILE)

#build: clean depend 
#	@make $(EXEC_FILE)

# compilation to native-code
$(EXEC_FILE): $(CMX_COMPILE)
	@echo ""
	@echo "**** Executable generation ****"
	$(OCAMLOPT) -o $(EXEC_FILE) $(PROFILE) $(INCLUDES) $(OCAML_NAT_LIB_DEPS) $(CMX_COMPILE)
	@echo ""
	@echo "*********************************************************"
	@echo "**** THE APPLICATION HAS BEEN SUCCESSFULLY GENERATED ****"
	@echo "*********************************************************"
	@echo ""
	@test -d $(BIN_DIR) || mkdir $(BIN_DIR)
	@mv $(EXEC_FILE) $(BIN_DIR)

# Generation of lexer and parser
#parser:
#	@echo "**** Generation of the parser ****"
#	@for f in $(YACC_SRC); do $(OCAMLYACC) -v $$f; done
#	@for f in $(LEX_SRC); do $(OCAMLLEX) $$f; done
#	@echo "**** Parsers Generation Complete ****"
#	@echo ""

# Compiles the C/OCAML interface modules
#interface: 
#	@echo "**** Compilation of C files ****"
#	@for f in $(C_FILES); do $(OCAMLOPT) -c $(OCAMLOPTFLAGS) $$f; done
#	@for f in $(SRC_FILES)$(O_FILES); do mv $$f $(OBJ_DIR); done

# Common Rules
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	@echo $@ >> .cmo.compile
	$(OCAMLC) -c $(OCAMLFLAGS) $<
	@mv $(SRC_DIR)/$@ $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmo=.cmi) $(OBJ_DIR)

.mli.cmi:
	@echo $@ >> .cmi.compile
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	@echo $@ >> .cmx.compile
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) $< 
	@mv $(SRC_DIR)/$@ $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.cmi) $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.o) $(OBJ_DIR)

.mll.cmx:
	$(OCAMLLEX) $<
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) $*.ml
	@mv $(SRC_DIR)/$@ $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.cmi) $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.o) $(OBJ_DIR)

.mly.cmi:
	$(OCAMLYACC) -v $<
	$(OCAMLC) $(OCAMLFLAGS) -c *.mli

.mly.cmx:
	$(OCAMLYACC) -v $<
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) $*.mli
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) $*.ml
	@mv $(SRC_DIR)/$@ $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.cmi) $(OBJ_DIR)
	@mv $(SRC_DIR)/$(@F:.cmx=.o) $(OBJ_DIR)

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

# Generates the modules dependencies
depend: $(SOURCES2) 
	@echo "**** Generation of dependencies ****"
	@test -d $(OBJ_DIR) || mkdir $(OBJ_DIR)
	@cd $(SRC_DIR)/ && $(OCAMLDEP) *.mli *.ml > ./.depend
	#@echo $(O_FILES) : $(subst $(SRC_DIR)/,,$(C_FILES)) > .cdepend
	#@cat .cdepend .mldepend > .depend
	@echo "**** Dependencies Generation Complete ****"
	@echo ""



#.c.o:
#	@echo $@ >> .o.compile
#	$(OCAMLOPT) $(OCAMLOPTFLAGS) $<
#	@mv $(SRC_DIR)/$@ $(OBJ_DIR)
#	@mv $(SRC_DIR)/$(@F:.c=.o) $(OBJ_DIR)


# Clean Up
clean:
	@echo "**** Cleaning Project ****"
	rm -f .cmx.compile
	rm -f .cmi.compile
	rm -f .cmo.compile
	rm -f .o.compile
#	rm -f .mldepend
#	rm -f .cdepend
	rm -f .depend
	rm -f *.output
#	rm -f $(LCI_LEXER_SRC)
#	rm -f $(LCI_PARSER).mli
#	rm -f $(LCI_PARSER).ml
	rm -rf $(OBJ_DIR)
	rm -rf $(BIN_DIR)
	rm -rf $(SRC_DIR)/*.cm[ixo]
	@echo "**** Cleaning Complete ****"
	@echo ""

# Generates a  
#build-debug: clean 
#	make parser
#	make depend 
#	make byte

# Compilation of a bytecode application
#byte: $(CMO_COMPILE)
#	@echo "**** Bytecode generation ****"
#	$(OCAMLC) -o $(EXEC_FILE) $(OCAMLFLAGS) $(OCAML_BYT_LIB_DEPS) $(CMO_COMPILE)
#	@test -d $(BIN_DIR) || mkdir $(BIN_DIR)
#	@mv $(EXEC_FILE) $(BIN_DIR)

include .depend
