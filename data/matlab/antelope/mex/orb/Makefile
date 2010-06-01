DATADIR=matlab/$(MATLAB_RELEASE)/antelope/antelope

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m)

INCLUDE= myorbxlat.h

HTMLDIR= matlab/html
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

include $(ANTELOPE)/local/include/mex_antelope_makerules
