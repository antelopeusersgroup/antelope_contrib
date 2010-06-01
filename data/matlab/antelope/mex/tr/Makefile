DATADIR=matlab/$(MATLAB_RELEASE)/antelope/antelope

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m)

HTMLDIR= matlab/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	trnew.c \
	trload_css.c \
	trload_cssgrp.c \
	trfree.c \
	trdestroy.c \
	trgetwf.c \
	trputwf.c \
	trsave_wf.c \
	trextract_data.c \
	trinsert_data.c \
	trapply_calib.c \
	trsplice.c \
	tr_nsamp.c \
	tr_endtime.c \
	tr_samprate.c \
	tr_time2samp.c \
	tr_samp2time.c \
	trwfname.c \
	trrotate.c \
	trrotate_to_standard.c \
	trfilter.c

CLEAN= 

cflags=
fflags=
ldflags=
ldlibs=

include $(ANTELOPE)/local/include/mex_antelope_makerules
