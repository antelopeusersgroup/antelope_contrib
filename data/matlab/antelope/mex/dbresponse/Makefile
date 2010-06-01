DATADIR=matlab/$(MATLAB_RELEASE)/antelope/antelope/@dbresponse

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m)

HTMLDIR= matlab/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	clear.c \
	dbresponse.c \
	compare_response.c \
	eval_response.c \
	parse_response.c \
	free_response.c 

include $(ANTELOPE)/local/include/mex_antelope_makerules
