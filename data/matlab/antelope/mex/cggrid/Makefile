DATADIR=matlab/antelope/antelope/@cggrid

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m) 

HTMLDIR= matlab/antelope/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	cggrid.c \
	cggrid_get.c \
	cggrid_getmesh.c \
	cggrid2db.c \
	cggrid_write.c \
	cggrid_free.c \
	clear.c 

CLEAN= 

cflags=
fflags=
ldflags=
ldlibs= 

include $(ANTELOPE)/include/mex_antelope_makerules
