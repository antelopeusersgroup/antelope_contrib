DATADIR=matlab/$(MATLAB_RELEASE)/antelope/antelope/@dbpf
DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m)

HTMLDIR= matlab/html
HTML= $(MEXSRCS:.c=.html)

INCLUDE=mypfxlat.h

MEXSRCS= \
	clear.c \
	dbpf.c \
	pf2string.c \
	pf2struct.c \
	pffree.c \
	pfget.c \
	pfget_arr.c \
	pfget_boolean.c \
	pfget_num.c \
	pfget_string.c \
	pfget_tbl.c \
	pfkeys.c \
	pfname.c \
	pfput.c \
	pfput_boolean.c \
	pfresolve.c \
	pftype.c \
	pfupdate.c \
	pfwrite.c

include $(ANTELOPE)/local/include/mex_antelope_makerules
