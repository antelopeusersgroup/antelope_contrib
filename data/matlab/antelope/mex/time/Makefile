DATADIR=matlab/$(MATLAB_RELEASE)/antelope/antelope
DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m)

HTMLDIR= matlab/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	str2epoch.c \
	epoch2str.c \
	yearday.c \
	strtime.c \
	strdate.c \
	strydtime.c \
	strtdelta.c \
	zepoch2str.c 

CLEAN= 

cflags=
fflags=
ldflags= 
ldlibs=

include $(ANTELOPE)/local/include/mex_antelope_makerules
