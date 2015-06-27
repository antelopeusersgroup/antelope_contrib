BIN  = dbdoc
PF   = $(BIN).pf
MAN1 = $(BIN).1
LICENSES = license_dbdoc.txt

DATA    = intro reldesc attrdesc reltables
DATADIR = doc/dbdoc

CLEAN= $(LICENSES)

ldlibs=	$(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

doc :  dbdoc
	dbdoc junk > dbdoc.out
	#tbl dbdoc.out | troff -ms -t | /usr/lib/lp/postscript/dpost > dbdoc.ps
	tbl dbdoc.out | ptroff -ms -t > dbdoc.ps

$(LICENSES): LICENSE
	cp $< $@
