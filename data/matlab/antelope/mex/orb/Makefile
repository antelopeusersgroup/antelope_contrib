DATADIR=matlab/antelope/antelope

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m)

INCLUDE= myorbxlat.h

HTMLDIR= matlab/antelope/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	orbopen.c \
	orbafter.c \
	orbselect.c \
	orbreject.c \
	orbget.c \
	orbreap.c \
	orbclose.c \
	orbtell.c \
	orbseek.c \
	orbping.c 

CLEAN= 

cflags=
fflags=
ldflags=
ldlibs=

include ../../antelope_mex_make
DBG += -argcheck

