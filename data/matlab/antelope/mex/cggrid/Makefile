DATADIR=matlab/$(MATLAB_RELEASE)/antelope/antelope/@cggrid

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m) \
      cggrid_free.m \
      clear.m 

HTMLDIR= matlab/html
HTML= $(MEXSRCS:.c=.html) \
      clear.html

MEXSRCS= \
	cggrid.c \
	cggrid2db.c \
	cggrid_dx.c \
	cggrid_dy.c \
	cggrid_get.c \
	cggrid_getmesh.c \
	cggrid_nx.c \
	cggrid_ny.c \
	cggrid_probe.c \
	cggrid_write.c 

CLEAN= 

cflags=
fflags=
ldflags=
ldlibs= 

include $(ANTELOPE)/local/include/mex_antelope_makerules
