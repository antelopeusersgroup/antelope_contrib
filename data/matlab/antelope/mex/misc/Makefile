DATADIR=matlab/antelope/antelope

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m) 

HTMLDIR= matlab/antelope/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	arrtimes.c \
	arr_slowness.c \
	cggrid_supported.c \
	clear_register.c \
	getpid.c \
	pffiles.c \
	parsepath.c

CLEAN= 

cflags=
fflags=
ldflags=
ldlibs= 

include $(ANTELOPE)/include/mex_antelope_makerules
