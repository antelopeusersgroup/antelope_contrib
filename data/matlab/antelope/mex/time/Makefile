DATADIR=matlab/antelope/antelope
DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m)

HTMLDIR= matlab/antelope/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	str2epoch.c \
	epoch2str.c \
	yearday.c \
	strtime.c \
	strdate.c \
	strydtime.c \
	strtdelta.c 

CLEAN= 

cflags=
fflags=
ldflags=
ldlibs=

include $(ANTELOPE)/include/mex_antelope_makerules
DBG += -argcheck

