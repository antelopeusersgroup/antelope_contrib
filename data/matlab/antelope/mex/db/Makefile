DATADIR=matlab/antelope/antelope

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m)

HTMLDIR= matlab/antelope/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	dbopen.c \
	dbclose.c \
	dbfree.c \
	dbadd.c \
	dbaddnull.c \
	dbaddv.c \
	dbgetv.c \
	dbputv.c \
	dbget.c \
	dbput.c \
	db2struct.c \
	dblookup.c \
	dblookup_table.c \
	dbnextid.c \
	dbjoin.c \
	dbjoin_keys.c \
	dbunjoin.c \
	dbnojoin.c \
	dbtheta.c \
	dbquery.c \
	dbsort.c \
	dbsubset.c \
	dbmark.c \
	dbcrunch.c \
	dbdelete.c \
	dbinvalid.c \
	dbfilename.c \
	dbextfile.c \
	dbeval.c \
	dbfind.c \
	dbadd_remark.c \
	dbget_remark.c \
	dbprocess.c \
	dbseparate.c \
	dbsever.c \
	dbnrecs.c \
	dbgroup.c \
	dbungroup.c

CLEAN= 

cflags=
fflags=
ldflags=
ldlibs=

include $(ANTELOPE)/include/mex_antelope_makerules

