DATADIR=matlab/antelope/antelope/@dbresponse

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m)

HTMLDIR= matlab/antelope/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	clear.c \
	dbresponse.c \
	eval_response.c \
	free_response.c 

include $(ANTELOPE)/include/mex_antelope_makerules
