DATADIR=matlab/$(MATLAB_RELEASE)/antelope/antelope

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m) 

HTMLDIR= matlab/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	abspath.c \
	arrtimes.c \
	arr_slowness.c \
	cggrid_supported.c \
	clear_register.c \
	concatpaths.c \
	datafile.c \
	datapath.c \
	getpid.c \
	pffiles.c \
	parsepath.c \
	relpath.c \
	elog_init.c \
	elog_log.c \
	elog_debug.c \
	elog_notify.c \
	elog_alert.c \
	elog_complain.c \
	elog_die.c \
	elog_mark.c \
	elog_clear.c \
	elog_string.c \
	elog_flush.c

CLEAN= 

cflags=
fflags=
ldflags=
ldlibs= 

include $(ANTELOPE)/local/include/mex_antelope_makerules
