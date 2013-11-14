EXTRALIBDIRS := stp/src/OcamlSTP
SOURCES := Main.ml Parser.ml Types.ml Constraints.ml Symbolic.ml
OBJS := $(filter %.cmo,$(SOURCES:.ml=.cmo))
CMIS := $(filter %.cmi,$(SOURCES:.mli=.cmi))
CSLFLAGS := -annot $(EXTRALIBDIRS:%=-I %)
CAMLP4_FLAGS := -pp "camlp4o"

default: mycute

mycute: $(OBJS)
	ocamlc $(CSLFLAGS) $(CAMLP4_FLAGS) OcamlSTP.cma OcamlSTPunsafe.cma Parser.cmo $^ -o mycute

$(OBJS) $(CMIS) : stp $(wildcard $(EXTRALIBDIRS) $(EXTRALIBDIRS:%=%/*))

clean:
	-$(RM) *.cmo *.cmi *.annot

distclean realclean: clean
	-$(MAKE) -C stp distclean


########################################
# implicit rules for ocaml compilation #
########################################

%.cmi : %.mli
	ocamlc $(CSLFLAGS) $(CAMLP4_FLAGS) -c $< -o $@

%.cmi : %.ml
	ocamlc $(CSLFLAGS) $(CAMLP4_FLAGS) -c $< -o $@

%.cmo : %.ml
	ocamlc $(CSLFLAGS) $(CAMLP4_FLAGS) -c $< -o $@

.dep: $(PARSER) $(SOURCES)
	ocamldep $(CAMLP4_FLAGS) *.ml* > .dep

depend: .dep

-include .dep

