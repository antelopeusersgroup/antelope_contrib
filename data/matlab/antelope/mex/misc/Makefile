DATADIR=matlab/antelope/antelope

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m) 

HTMLDIR= matlab/antelope/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	arrtimes.c \
	clear_register.c \
	pffiles.c 

CLEAN= 

cflags=
fflags=
ldflags=
ldlibs=

include $(ANTELOPE)/include/mex_antelope_makerules
DBG += -argcheck

