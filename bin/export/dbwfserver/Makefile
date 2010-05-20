BIN = dbwfserver
PF = dbwfserver.pf
MAN1 = dbwfserver.1
DATADIR = python/dbwfserver
DATA = __init__.py \
       server.py \
       resource.py \
       eventdata.py \
       config.py 

CLEAN = *.pyc
UNINSTALL = $(ANTELOPE)/data/$(DATADIR)/*.pyc

include $(ANTELOPEMAKE)

DIRS = Contents

.SUFFIXES: .SUFFIXES .xpy

% : %.xpy $(ANTELOPE)/data/templates/xpy
	cat $(ANTELOPE)/data/templates/xpy $< > $@
	chmod a+x $@
