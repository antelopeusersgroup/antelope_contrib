BIN  = dbwfserver_extract
BIN += dbwfserver
PF =  dbwfserver.pf
MAN1 = dbwfserver.1

ldlibs = -lxtra $(TRLIBS) $(GPLLIBS)
SUBDIR = /contrib
CLEAN = *.pyc *.o $(BIN)
UNINSTALL = $(ANTELOPE)$(SUBDIR)/data/$(DATADIR)/*.pyc $(ANTELOPE)$(SUBDIR)/data/$(DATADIR)/*.pyc


include $(ANTELOPEMAKE)

DIRS  = Contents
DIRS += pydbwfserver