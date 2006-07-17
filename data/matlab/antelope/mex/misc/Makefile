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
	parsepath.c \
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

include $(ANTELOPE)/include/mex_antelope_makerules
