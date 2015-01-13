BIN=dbdoc
PF=dbdoc.pf
DATA=intro reldesc attrdesc reltables
DATADIR=doc/dbdoc

MAN1=dbdoc.1

CLEAN=

ldlibs=	$(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

doc :  dbdoc
	dbdoc junk > dbdoc.out
	#tbl dbdoc.out | troff -ms -t | /usr/lib/lp/postscript/dpost > dbdoc.ps
	tbl dbdoc.out | ptroff -ms -t > dbdoc.ps
