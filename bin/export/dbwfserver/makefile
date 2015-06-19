BIN = dbwfserver_extract dbwfserver
PF =  dbwfserver.pf
MAN1 = dbwfserver.1

DATADIR = python/dbwfserver

DATA = __init__.py \
       server.py \
       resource.py \
       dbcentral.py \
       config.py

ldlibs=-lxtra $(TRLIBS) $(GPLLIBS)
SUBDIR=/contrib
CLEAN = *.pyc *.o $(BIN)
UNINSTALL = $(ANTELOPE)$(SUBDIR)/data/$(DATADIR)/*.pyc $(ANTELOPE)$(SUBDIR)/data/$(DATADIR)/*.pyc
DIRS = Contents

include $(ANTELOPEMAKE)
