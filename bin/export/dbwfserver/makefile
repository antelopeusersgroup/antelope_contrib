BIN = dbwfserver_extract dbwfserver
PF =  dbwfserver.pf
MAN1 = dbwfserver.1

DATADIR = python/dbwfserver

DATA = __init__.py \
       server.py \
       resource.py \
       dbcentral.py \
       config.py

CLEAN = *.pyc *.o
UNINSTALL = $(ANTELOPE)/data/$(DATADIR)/*.pyc $(ANTELOPE)/data/$(DATADIR)/*.pyc

ldlibs=-lxtra $(TRLIBS) $(GPLLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS = Contents
