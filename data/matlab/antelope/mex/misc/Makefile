DATADIR=matlab/antelope/antelope

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m) 

HTMLDIR= matlab/antelope/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	clear_register.c \
	pffiles.c 

CLEAN= 

cflags=
fflags=
ldflags=
ldlibs=

include ../../antelope_mex_make
DBG += -argcheck

