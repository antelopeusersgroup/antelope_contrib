
BIN= orbtopo
PF= orbtopo.pf

MAN1= orbtopo.1

include $(ANTELOPEMAKE)

DIRS=


# SCAFFOLD until full python make structures are in place:

.SUFFIXES: .SUFFIXES .xpy

% : %.xpy $(ANTELOPE)/data/templates/xpy
	cat $(ANTELOPE)/data/templates/xpy $< > $@
	chmod a+x $@
